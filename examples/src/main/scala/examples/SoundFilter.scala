package examples

import javax.sound.sampled.{AudioSystem, AudioFormat, SourceDataLine, DataLine}
import java.io.File
import java.io.ByteArrayInputStream
import java.nio.file.Files
import org.functionaljustin.duct.datatypes.NonEmptyLazyList

object SoundFilter:
  // Read the entire file into a NonEmptyLazyList[Byte]
  def readToNonEmptyLazyListFromFile(file: File): NonEmptyLazyList[Byte] =
    val bytes = Files.readAllBytes(file.toPath)
    assert(bytes.nonEmpty, "Input file cannot be empty for NonEmptyLazyList.")
    NonEmptyLazyList.unfold(0) { currentIndex =>
      val element = bytes(currentIndex)
      val nextIndex = currentIndex + 1
      (element, nextIndex)
    }.take(bytes.length)

  // Start with a simple hello world program
  def main(args: Array[String]): Unit =
    val sampleFile = if args.length > 0 then args(0) else "/Users/justinhj/Downloads/6894__timbre__vintage-78-rpm-style/108373__timbre__vintage-in-78-rpm-style.wav"
    // The compiler handles checking if an argument is provided and parsing it as a String
    println(s"Loading sample ${sampleFile}")

    // Read file into NonEmptyLazyList[Byte]
    val file = new File(sampleFile)
    val len = file.length()
    println(s"File length is ${len} bytes")

    // val nel = readToNonEmptyLazyListFromFile(file)
    // val bytes = nel.toLazyList.take(4097).toList
    // println(s"First 256 bytes: ${bytes.mkString(",")}")
    // val bytes = nel.take(len.toInt).toList.toArray
    // val audioInputStream = AudioSystem.getAudioInputStream(new ByteArrayInputStream(bytes))
    // val audioFormat = audioInputStream.getFormat
    // println(s"Audio format: ${audioFormat}")

    // // Prepare SourceDataLine for playback
    // val info = new DataLine.Info(classOf[SourceDataLine], audioFormat)
    // val line = AudioSystem.getLine(info).asInstanceOf[SourceDataLine]
    // line.open(audioFormat)
    // line.start()

    // val chunkSize = 256
    // var totalBytes = 0L
    
    // while(totalBytes < 1_000_000)
    //   val chunk = nel.take(chunkSize).toList.toArray
    //   line.write(chunk, 0, chunk.length)
    //   totalBytes += chunk.length

    // Clean up
    // line.drain()
    // line.stop()
    // line.close()
    // audioInputStream.close()
    // println(s"Playback finished. Total bytes played: $totalBytes")
