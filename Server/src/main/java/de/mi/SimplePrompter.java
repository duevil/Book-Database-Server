package de.mi;

import javax.swing.*;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.Charset;

public final class SimplePrompter {
    private SimplePrompter() {
    }

    public static String getConsoleInput(String prompt) {
        String input;
        System.out.println(prompt);
        try (var os = new ByteArrayOutputStream()) {
            do os.write(System.in.read());
            while (System.in.available() > 0);
            input = os.toString(Charset.defaultCharset()).strip();
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
        return input;
    }

    public static String getDialogInput(String prompt) {
        return JOptionPane.showInputDialog(prompt);
    }
}
