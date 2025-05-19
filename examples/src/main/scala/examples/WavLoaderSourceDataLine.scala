package examples

import javax.sound.sampled._
import java.io.{File, IOException, ByteArrayOutputStream}
import scala.util.{Try, Using}
import java.io.File
import java.io.ByteArrayInputStream
import java.nio.file.Files
import org.functionaljustin.duct.datatypes.NonEmptyLazyList

object WavLoaderSourceDataLine {
  def readToNonEmptyLazyListFromFile(file: File): NonEmptyLazyList[Byte] =
    val bytes = Files.readAllBytes(file.toPath)
    assert(bytes.nonEmpty, "Input file cannot be empty for NonEmptyLazyList.")
    NonEmptyLazyList
      .unfold(0) { currentIndex =>
        val element = bytes(currentIndex)
        val nextIndex = currentIndex + 1
        (element, nextIndex)
      }
      .take(bytes.length)

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
        return
      }

      Using(AudioSystem.getLine(info).asInstanceOf[SourceDataLine]) {
        sourceDataLine =>
          // Open the line and start playback
          sourceDataLine.open(audioFormat)
          sourceDataLine.start()

          // Write the audio data to the line
          sourceDataLine.write(audioData, 0, audioData.length)

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
