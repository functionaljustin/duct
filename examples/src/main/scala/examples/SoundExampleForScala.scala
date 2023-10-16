package examples

import javax.sound.sampled.*;
import java.io.ByteArrayInputStream;
import java.io.IOException;

object SoundExampleForScala:
  def main(args: Array[String]): Unit = {
    // Parse the duration from the command-line arguments
    val duration: Int = if (args.length > 0) args(0).toInt else 10

    // Set the audio format and parameters
    val format = new AudioFormat(44100, 16, 1, true, false)
    val sampleSize = (format.getSampleRate * format.getFrameSize).toInt

    // Create a sine wave generator function
    def sineWave(frequency: Double, duration: Double): Array[Double] = {
      val numSamples = (duration * format.getSampleRate).toInt
      val samples = new Array[Double](numSamples)
      for (i <- 0 until numSamples) {
        samples(i) = math.sin(2 * math.Pi * i * frequency / format.getSampleRate)
      }
      samples
    }

    // Generate a sine wave with the specified duration at 440 Hz
    val samples = sineWave(440, duration)

    // Convert the Array[Double] array to a Array[Byte] array
    val bytes = new Array[Byte](sampleSize * duration)
    for (i <- samples.indices) {
      val value = (samples(i) * 32767).toInt
      bytes(i * 2) = (value & 0xFF).toByte
      bytes(i * 2 + 1) = ((value >> 8) & 0xFF).toByte
    }

    // Create a new audio input stream from the Array[Byte] array
    val stream = new AudioInputStream(new ByteArrayInputStream(bytes), format, bytes.length / format.getFrameSize)

    // Play the audio stream using the AudioSystem class
    try {
      val clip = AudioSystem.getClip()
      clip.open(stream)
      clip.start()
      Thread.sleep((duration * 1000).toInt) // Wait for the audio to finish playing
    } catch {
      case e: LineUnavailableException => e.printStackTrace()
      case e: InterruptedException => e.printStackTrace()
    }
  }
