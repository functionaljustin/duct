package examples
import javax.sound.sampled._

object ScalaStreamingAudio extends App:
  // Set the audio format and parameters
  val format = new AudioFormat(44100, 16, 1, true, false)
  val sampleSize = (format.getSampleRate * format.getFrameSize).toInt
  val bufferSize = sampleSize * 2

  // Create a sine wave generator function
  def sineWave(frequency: Double, amplitude: Double, phase: Double): Double = {
    amplitude * math.sin(2 * math.Pi * frequency / format.getSampleRate + phase)
  }

  // Open an audio dataline for writing
  val line = AudioSystem.getSourceDataLine(format)
  line.open(format, bufferSize)
  line.start()

  // Initialize the phase and frequency of the sine wave
  var phase = 0.0
  var frequency = 440.0

  // Generate and write the sine wave to the audio dataline in a loop
  // TODO this should now process the audio using a filter via my comonad code
  while (true) {
    val buffer = new Array[Byte](bufferSize)
    for (i <- 0 until bufferSize / 2) {
      val value = (sineWave(frequency, 32767, phase) * 0.5).toInt
      buffer(i * 2) = (value & 0xFF).toByte
      buffer(i * 2 + 1) = ((value >> 8) & 0xFF).toByte
      phase += 2 * math.Pi * frequency / format.getSampleRate
    }
    line.write(buffer, 0, bufferSize)
  }
