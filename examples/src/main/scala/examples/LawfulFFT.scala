/*
 * LawfulFFT — a real-time spectrum display whose core is a comonad.
 *
 * Ideas for subsequent video/post:
 *
 *   `samples.coflatMap(spectrumAt)` denotes "the spectrum at every instant"
 *   as a single timeless, pure value. It says nothing about microphones,
 *   buffer sizes, or redraw timing. The driver loop at the bottom — blocking
 *   on the mic, advancing by hops, redrawing the screen — lives deliberately
 *   OUTSIDE the comonad. The comonad gives you the denotation; the driver
 *   gives you the pacing.
 *
 * Uses the Comonad[NonEmptyLazyList] instance from duct (extract = head,
 * coflatMap via tails — the suffix semantics). No other dependencies: audio
 * capture is plain javax.sound.sampled and the TUI is raw ANSI escapes.
 *
 * Run:   sbt "examples/runMain examples.LawfulFFT"
 *        sbt "examples/runMain examples.LawfulFFT --test-tone"
 * Quit:  q + Enter, or Ctrl-C. (macOS will ask for mic permission once.)
 */

package examples

import org.functionaljustin.duct.datatypes.NonEmptyLazyList
import org.functionaljustin.duct.typeclasses.comonad.{given, _}

// ---------------------------------------------------------------------------
// Signal processing: hand-rolled Cooley–Tukey (~35 lines), Hann window,
//    log-spaced bars on a dB scale.
// ---------------------------------------------------------------------------

object FFT:
  /** Real-input FFT; returns linear magnitudes of the first n/2 bins. */
  def magnitudeSpectrum(samples: Array[Double]): Array[Double] =
    val n = samples.length
    require((n & (n - 1)) == 0, "window size must be a power of two")
    val re = samples.clone()
    val im = new Array[Double](n)

    var j = 0
    var i = 1
    while i < n do // bit-reversal permutation
      var bit = n >> 1
      while (j & bit) != 0 do { j ^= bit; bit >>= 1 }
      j ^= bit
      if i < j then
        val t = re(i); re(i) = re(j); re(j) = t
      i += 1

    var len = 2
    while len <= n do // butterflies
      val ang = -2.0 * math.Pi / len
      val wr = math.cos(ang)
      val wi = math.sin(ang)
      var k = 0
      while k < n do
        var cwr = 1.0
        var cwi = 0.0
        var m = 0
        while m < len / 2 do
          val ur = re(k + m)
          val ui = im(k + m)
          val vr = re(k + m + len / 2) * cwr - im(k + m + len / 2) * cwi
          val vi = re(k + m + len / 2) * cwi + im(k + m + len / 2) * cwr
          re(k + m) = ur + vr
          im(k + m) = ui + vi
          re(k + m + len / 2) = ur - vr
          im(k + m + len / 2) = ui - vi
          val nwr = cwr * wr - cwi * wi
          cwi = cwr * wi + cwi * wr
          cwr = nwr
          m += 1
        k += len
      len <<= 1

    val out = new Array[Double](n / 2)
    var b = 0
    while b < n / 2 do
      out(b) = math.sqrt(re(b) * re(b) + im(b) * im(b)) / n
      b += 1
    out

val WindowSize = 2048
val Hop        = 1024
val Bars       = 64
val Rows       = 20

/** The context-dependent local computation: window + FFT of one suffix. */
def spectrumAt(suffix: NonEmptyLazyList[Double]): Array[Double] =
  val buf = new Array[Double](WindowSize)
  var s = suffix
  var i = 0
  while i < WindowSize do
    val hann = 0.5 * (1.0 - math.cos(2.0 * math.Pi * i / (WindowSize - 1)))
    buf(i) = s.head * hann
    s = s.tailOption.getOrElse(s) // finite stream: hold the last sample
    i += 1
  toBars(FFT.magnitudeSpectrum(buf), Bars)

/** Log-spaced bin aggregation, -90..0 dB mapped to 0..1. */
def toBars(mags: Array[Double], bars: Int): Array[Double] =
  val out = new Array[Double](bars)
  var b = 0
  while b < bars do
    val lo = math.pow(mags.length, b.toDouble / bars).toInt.min(mags.length - 1)
    val hi = math.pow(mags.length, (b + 1).toDouble / bars).toInt.max(lo + 1).min(mags.length)
    var peak = 0.0
    var k = lo
    while k < hi do
      if mags(k) > peak then peak = mags(k)
      k += 1
    val db = 20.0 * math.log10(peak + 1e-12)
    out(b) = ((db + 90.0) / 90.0).min(1.0).max(0.0)
    b += 1
  out

// ---------------------------------------------------------------------------
// 4. Sources: the mic (javax.sound.sampled, no deps) or a synth test tone.
//    Both are infinite NonEmptyLazyList[Double] — the comonad can't tell them apart,
//    which is exactly the denotation/operation split doing its job.
// ---------------------------------------------------------------------------

object Mic:
  import javax.sound.sampled.*

  def openStream(sampleRateHz: Float = 44100f): NonEmptyLazyList[Double] =
    val fmt = new AudioFormat(sampleRateHz, 16, 1, true, false)
    val line =
      try AudioSystem.getTargetDataLine(fmt)
      catch
        case e: Exception =>
          System.err.println(s"no microphone available (${e.getMessage}); try --test-tone")
          sys.exit(1)
    line.open(fmt, 16384)
    line.start()
    Runtime.getRuntime.addShutdownHook(new Thread(() => { line.stop(); line.close() }))

    val queue = new java.util.concurrent.LinkedBlockingQueue[Double](1 << 16)
    val feeder = new Thread(() =>
      val buf = new Array[Byte](4096)
      var go = true
      while go do
        try
          val n = line.read(buf, 0, buf.length)
          if n <= 0 then go = false
          else
            var i = 0
            while i + 1 < n do
              val lo = buf(i) & 0xff
              val hi = buf(i + 1) // widened: sign-extended
              queue.put((((hi << 8) | lo).toShort) / 32768.0)
              i += 2
        catch case _: InterruptedException => go = false
    , "mic-feeder")
    feeder.setDaemon(true)
    feeder.start()

    def pull(): NonEmptyLazyList[Double] =
      NonEmptyLazyList.cons(queue.take(), pull().toLazyList) // take() blocks: backpressure for free
    pull()

/** Deterministic synth signal — same type as the mic, for demos without one. */
def testToneStream(sampleRateHz: Double): NonEmptyLazyList[Double] =
  def go(t: Long): NonEmptyLazyList[Double] =
    val x = t / sampleRateHz
    val sweepHz = 200 + 600 * (0.5 + 0.5 * math.sin(2 * math.Pi * 0.1 * x))
    val v = 0.45 * math.sin(2 * math.Pi * 220 * x)
          + 0.30 * math.sin(2 * math.Pi * 440 * x)
          + 0.25 * math.sin(2 * math.Pi * sweepHz * x)
    NonEmptyLazyList.cons(v, go(t + 1).toLazyList)
  go(0)

// ---------------------------------------------------------------------------
// 5. The "TUI library": six escape codes and the block characters.
//    Alternate screen + hidden cursor; restored on exit come what may.
// ---------------------------------------------------------------------------

object TUI:
  def enter(): Unit =
    print("\u001b[?1049h\u001b[?25l")
    System.out.flush()
  def exit(): Unit =
    print("\u001b[?25h\u001b[?1049l")
    System.out.flush()
  def render(bars: Array[Double], rows: Int): Unit =
    val sb = new StringBuilder("\u001b[H")
    sb.append("lawful fft — comonad denotation, driver pacing   (q + Enter to quit)\n")
    var r = rows - 1
    while r >= 0 do
      val thresh = r.toDouble / rows
      var b = 0
      while b < bars.length do
        sb.append(if bars(b) >= thresh then '█' else ' ')
        b += 1
      sb.append('\n')
      r -= 1
    print(sb.toString())
    System.out.flush()

def advance[A](s: NonEmptyLazyList[A], n: Int): NonEmptyLazyList[A] =
  var cur = s
  var i = 0
  while i < n do
    cur = cur.tailOption.getOrElse(cur)
    i += 1
  cur

// ---------------------------------------------------------------------------
// 6. The driver: everything operational lives here, outside the comonad.
// ---------------------------------------------------------------------------

object LawfulFFT:


  def main(args: Array[String]): Unit =


    val useTestTone = args.contains("--test-tone")

    // THE LAWFUL CORE, one line: the spectrum at every instant, timelessly.
    // Rebuilt per frame from the advanced sample cursor: `NonEmptyLazyList.map` is
    // head-strict (cons takes its head by value), so stepping the *spectra*
    // stream would force one FFT per hop — 1024 wasted FFTs per frame.
    // Stepping the *sample* cursor and forcing `.head` evaluates exactly the
    // spectrum displayed and nothing else; the rest of the infinite
    // denotation is never evaluated.
    // NOTE: keep only the current sample cursor in `samples`. Tails are
    // memoized, so retaining the stream root would pin every consumed sample
    // forever; advancing the cursor lets the prefix be GC'd.
    var samples: NonEmptyLazyList[Double] =
      if useTestTone then testToneStream(44100.0)
      else Mic.openStream(44100f)

    @volatile var running = true
    val keys = new Thread(() =>
      val in = new java.util.Scanner(System.in)
      while running && in.hasNextLine() do
        if in.nextLine().trim.equalsIgnoreCase("q") then running = false
    , "key-reader")
    keys.setDaemon(true)
    keys.start()

    TUI.enter()
    Runtime.getRuntime.addShutdownHook(new Thread(() => TUI.exit()))
    try
      while running do
        val spectra = samples.coflatMap(spectrumAt) // lawful core, rebuilt per frame
        TUI.render(spectra.head, Rows) // the only spectrum we ever force
        samples = advance(samples, Hop) // hop the samples, never look back
        // The mic stream self-throttles (queue.take() blocks), but the synth
        // stream never blocks — without pacing, test-tone mode would render as
        // fast as the terminal allows and spin the CPU. Pace it like mic mode.
        if useTestTone then Thread.sleep(23)
    finally
      running = false
      TUI.exit()

