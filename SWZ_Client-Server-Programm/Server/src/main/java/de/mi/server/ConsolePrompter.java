package de.mi.server;

import java.io.IOException;
import java.io.UncheckedIOException;

/**
 * Utility-Klasse zum Anzeigen einer einfachen Prompt-Nachricht und dem Erwarten einer Eingabe in der Konsole
 *
 * @author Malte Kasolowsky <code>m30114</code>
 * @deprecated not possible with docker
 */
@Deprecated(since = "2.0") // console input not possible with docker
public final class ConsolePrompter {
    /**
     * Privater Konstruktor; eine Erzeugung einer Klassen-Instanz ist nicht nötig
     */
    private ConsolePrompter() {
    }

    /**
     * Schreibt die gegebene Prompt-Nachricht in {@link System#out}
     * und wartet auf eine Eingabe des Nutzers in {@link System#in},
     * welche mittels der Enter-Taste gesendet und ausgelesen wird
     *
     * @param prompt Die anzuzeigen Kommandozeile
     * @return Die Eingabe
     */
    public static String prompt(String prompt) {
        try {
            System.out.println(prompt);
            var buffer = new StringBuilder();
            do buffer.appendCodePoint(System.in.read());
            while (System.in.available() > 0);
            return buffer.toString().strip();
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }
}
