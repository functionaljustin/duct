package examples;

import javax.sound.sampled.AudioInputStream;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.AudioFormat;
import javax.sound.sampled.DataLine;
import javax.sound.sampled.Clip;
import java.io.File;
import java.io.IOException;
import javax.sound.sampled.LineUnavailableException;
import javax.sound.sampled.UnsupportedAudioFileException;
import java.io.ByteArrayInputStream;

public class SoundExampleWithClip {

    public static void main(String[] args) {
        File wavFile = new File("/Users/justinhj/Downloads/6894__timbre__vintage-78-rpm-style/108373__timbre__vintage-in-78-rpm-style.wav"); // Replace with your file path
        AudioInputStream audioInputStream = null;
        Clip clip = null;

        try {
            audioInputStream = AudioSystem.getAudioInputStream(wavFile);
            System.out.println("WAV file loaded successfully.");

            AudioFormat audioFormat = audioInputStream.getFormat();
            System.out.println("Audio Format:");
            System.out.println("  Encoding: " + audioFormat.getEncoding());
            System.out.println("  Sample Rate: " + audioFormat.getSampleRate() + " Hz");
            System.out.println("  Sample Size in Bits: " + audioFormat.getSampleSizeInBits());
            System.out.println("  Channels: " + audioFormat.getChannels());
            System.out.println("  Frame Size: " + audioFormat.getFrameSize() + " bytes");
            System.out.println("  Frame Rate: " + audioFormat.getFrameRate() + " Hz");
            System.out.println("  Big Endian: " + audioFormat.isBigEndian());

            // Get a Clip
            DataLine.Info info = new DataLine.Info(Clip.class, audioFormat);
            if (!AudioSystem.isLineSupported(info)) {
                System.err.println("Clip not supported for the given audio format.");
                return;
            }
            clip = (Clip) AudioSystem.getLine(info);

            // Open the Clip and load the audio data
            clip.open(audioInputStream);

            // Start playback
            clip.start();

            // Keep the program alive while the clip is playing (for demonstration)
            while (!clip.isRunning()) {
                Thread.sleep(10);
            }
            while (clip.isRunning()) {
                Thread.sleep(10);
            }

        } catch (UnsupportedAudioFileException e) {
            System.err.println("Unsupported audio file format: " + e.getMessage());
        } catch (IOException e) {
            System.err.println("Error reading WAV file: " + e.getMessage());
        } catch (LineUnavailableException e) {
            System.err.println("Audio line unavailable: " + e.getMessage());
        } catch (InterruptedException e) {
            System.err.println("Playback interrupted: " + e.getMessage());
        } finally {
            // Close resources
            if (clip != null) {
                clip.close();
            }
            if (audioInputStream != null) {
                try {
                    audioInputStream.close();
                } catch (IOException e) {
                    System.err.println("Error closing audio input stream: " + e.getMessage());
                }
            }
        }
    }
}
