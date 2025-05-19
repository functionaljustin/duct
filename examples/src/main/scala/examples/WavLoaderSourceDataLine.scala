package examples

import javax.sound.sampled._
import java.io.{ByteArrayOutputStream, IOException}
import scala.util.{Try, Using}
import java.io.File
import java.nio.file.Files
import org.functionaljustin.duct.datatypes.NonEmptyLazyList

object WavLoaderSourceDataLine {
  def readToNonEmptyLazyListFromFile(input: Array[Byte]): NonEmptyLazyList[Byte] = {
    assert(input.nonEmpty, "Input array cannot be empty for NonEmptyLazyList.")
    NonEmptyLazyList
      .unfold(0) { currentIndex =>
        val element = input(currentIndex)
        val nextIndex = (currentIndex + 1) % input.length
        (element, nextIndex)
      }
  }

  def main(args: Array[String]): Unit = {
    val wavFilePath =
      "/Users/justinhj/Downloads/6894__timbre__vintage-78-rpm-style/108373__timbre__vintage-in-78-rpm-style.wav" // Replace with your file path
    val wavFile = new File(wavFilePath)

    Using(AudioSystem.getAudioInputStream(wavFile)) { audioInputStream =>
      println("WAV file loaded successfully.")

      val audioFormat = audioInputStream.getFormat
      println("Audio Format:")
      println(s"  Encoding: ${audioFormat.getEncoding}")
      println(s"  Sample Rate: ${audioFormat.getSampleRate} Hz")
      println(s"  Sample Size in Bits: ${audioFormat.getSampleSizeInBits}")
      println(s"  Channels: ${audioFormat.getChannels}")
      println(s"  Frame Size: ${audioFormat.getFrameSize} bytes")
      println(s"  Frame Rate: ${audioFormat.getFrameRate} Hz")
      println(s"  Big Endian: ${audioFormat.isBigEndian}")

      // Read audio data into a byte array
      val byteArrayOutputStream = new ByteArrayOutputStream()
      val buffer = new Array[Byte](4096) // Buffer size
      var bytesRead = 0
      while ({ bytesRead = audioInputStream.read(buffer); bytesRead != -1 }) {
        byteArrayOutputStream.write(buffer, 0, bytesRead)
      }
      val audioData = byteArrayOutputStream.toByteArray
      println(
        s"Audio data read into a byte array of size: ${audioData.length} bytes"
      )

      // Get a SourceDataLine for playback
      val info = new DataLine.Info(classOf[SourceDataLine], audioFormat)
      if (!AudioSystem.isLineSupported(info)) {
        println("Line not supported for the given audio format.")
        System.exit(1)
      }

      Using(AudioSystem.getLine(info).asInstanceOf[SourceDataLine]) {
        sourceDataLine =>
          // Open the line and start playback
          sourceDataLine.open(audioFormat)
          sourceDataLine.start()

          // Create NonEmptyLazyList from audioData
          val audioStream = readToNonEmptyLazyListFromFile(audioData)
          val chunkSize = 256 // Same as buffer size
          var loopCount = 0
          val maxLoops = 10 // Number of times to loop the audio

          // Function to process chunks and loop
          def playLoop(currentStream: NonEmptyLazyList[Byte], loops: Int): Unit = {
            if (loops >= maxLoops) return // Stop after maxLoops

            // Take a chunk of bytes
            val chunk = currentStream.take(chunkSize).toList.toArray
            sourceDataLine.write(chunk, 0, chunk.length)

            // Move to next chunk
            val nextStream = currentStream.drop(chunkSize)
            playLoop(nextStream, loops)
          }

          // Start playback loop
          playLoop(audioStream, 0)

          // Wait for the playback to complete
          sourceDataLine.drain()

          println("Playback finished.")

      } match {
        case scala.util.Success(_) => // Line closed successfully
        case scala.util.Failure(e: LineUnavailableException) =>
          System.err.println(s"Audio line unavailable: ${e.getMessage}")
        case scala.util.Failure(e) =>
          System.err.println(
            s"An error occurred during playback: ${e.getMessage}"
          )
      }

    } match {
      case scala.util.Success(_) => // Audio stream closed successfully
      case scala.util.Failure(e: UnsupportedAudioFileException) =>
        System.err.println(s"Unsupported audio file format: ${e.getMessage}")
      case scala.util.Failure(e: IOException) =>
        System.err.println(s"Error reading WAV file: ${e.getMessage}")
      case scala.util.Failure(e) =>
        System.err.println(s"An unexpected error occurred: ${e.getMessage}")
    }
  }
}