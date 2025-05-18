package examples

import javax.sound.sampled.{AudioSystem, AudioFormat, SourceDataLine, DataLine}
import java.io.File
import org.functionaljustin.duct.datatypes.NonEmptyLazyList

object SoundFilter:

  // Using the NonEmptyLazyList to read the audio stream
  // Means write a function that creates a closure and uses unfold to 
  // read the elements of the stream, it should loop over them
  // Note there is no way to release the stream and it is dependent on the caller
  def readToNonEmptyLazyList(stream: java.io.InputStream): NonEmptyLazyList[Byte] =
    val bytes = stream.readAllBytes()
    assert(bytes.nonEmpty, "Input stream cannot be empty for NonEmptyLazyList.")

    NonEmptyLazyList.unfold(0) { currentIndex =>
      val element = bytes(currentIndex)
      val nextIndex = (currentIndex + 1) % bytes.length
      (element, nextIndex)
    }

  // Start with a simple hello world program
  def main(args: Array[String]): Unit =
    val sampleFile = if args.length > 0 then args(0) else "/Users/justinhj/Downloads/6894__timbre__vintage-78-rpm-style/108373__timbre__vintage-in-78-rpm-style.wav"
    // The compiler handles checking if an argument is provided and parsing it as a String
    println(s"Loading sample ${sampleFile}")

    // Load audio input stream
    val audioInputStream = AudioSystem.getAudioInputStream(new File(sampleFile))
    val audioFormat = audioInputStream.getFormat
    println(s"Audio format: ${audioFormat}")

    // Prepare SourceDataLine for playback
    val info = new DataLine.Info(classOf[SourceDataLine], audioFormat)
    val line = AudioSystem.getLine(info).asInstanceOf[SourceDataLine]
    line.open(audioFormat)
    line.start()

    // Buffer for reading and processing audio
    val buffer = new Array[Byte](4096)
    var bytesRead = 0
    var totalBytes = 0L

    // Read, (optionally process), and play audio
    while {
      bytesRead = audioInputStream.read(buffer, 0, buffer.length)
      bytesRead != -1
    } do
      // Implement a min filter
      val what = buffer.take(bytesRead).map(b => {
        // min filter, clip the byte to -20 to 20
        // if(b > -20 && b < 20) {
        //   0
        // } else b
        b

      })       
      line.write(what, 0, bytesRead)
      totalBytes += bytesRead

    // Clean up
    line.drain()
    line.stop()
    line.close()
    audioInputStream.close()
    println(s"Playback finished. Total bytes played: $totalBytes")
