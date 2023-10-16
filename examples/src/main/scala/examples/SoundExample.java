package examples;

import javax.sound.sampled.*;
import java.io.ByteArrayInputStream;
import java.io.IOException;

// Java program created by OpenAI to generate a sine wave and play it using the javax sound library
// Idea is to extend this to play a continuous stream and modify it using comonadic filters
// Also convert it to Scala
class SoundExample {

    // Set the audio format and parameters
    static AudioFormat format = new AudioFormat(44100, 16, 1, true, false);
    static int sampleSize = (int) format.getSampleRate() * format.getFrameSize();

    // Create a sine wave generator function
    static double[] sineWave(double frequency, double duration) {
        int numSamples = (int) (duration * format.getSampleRate());
        double[] samples = new double[numSamples];
        for (int i = 0; i < numSamples; i++) {
            samples[i] = Math.sin(2 * Math.PI * i * frequency / format.getSampleRate());
        }
        return samples;
    }

    public static void main(String[] args) {
        int seconds = 3; // The number of seconds to play the sound

        // Generate sine wave at the specified Hz
        double[] samples = SoundExample.sineWave(130, seconds);

        System.out.println(samples.length + ", " + sampleSize);

        // Convert the double[] array to a byte[] array
        byte[] bytes = new byte[sampleSize * seconds];
        for (int i = 0; i < samples.length; i++) {
            int value = (int) (samples[i] * 32767);
            bytes[i * 2] = (byte) (value & 0xFF);
            bytes[i * 2 + 1] = (byte) ((value >> 8) & 0xFF);
        }

        // Create a new audio input stream from the byte array
        AudioInputStream stream = new AudioInputStream(new ByteArrayInputStream(bytes), format,
                bytes.length / format.getFrameSize());

        // Play the audio stream using the AudioSystem class
        try {
            Clip clip = AudioSystem.getClip();
            clip.open(stream);
            clip.start();
            Thread.sleep(5000); // Wait for the audio to finish playing
        } catch (IOException | LineUnavailableException | InterruptedException e) {
            e.printStackTrace();
        }
    }

}