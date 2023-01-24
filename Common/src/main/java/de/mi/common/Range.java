package de.mi.common;

/**
 * Einfach Datenklasse zum Speichern eines Wertebereichs
 *
 * @param min Der minimale erlaubte Wert
 * @param max Der maximale erlaubte Wert
 * @author Malte Kasolowsky <code>m30114</code>
 */
public record Range(int min, int max) {
    /**
     * Prüft, ob der minimale Wert kleiner gleich dem maximalen Wert ist,
     * und wirft andernfalls eine {@link IllegalRangeException}
     *
     * @throws IllegalRangeException Wenn der untere angegebene Wert des Bereiches über dem oberen Wert liegt
     */
    public Range {
        if (min > max) throw new IllegalRangeException(min, max);
    }

    /**
     * Prüft, ob ein Wert innerhalb dieses Bereiches liegt,
     * und wirft andernfalls eine entsprechende Ausnahme
     *
     * @param i Der zu prüfende Wert
     * @return Der geprüfte Wert, sofern er im Bereich liegt
     * @throws OutOfRangeException Wenn der Wert außerhalb des Bereiches liegt,
     *                             d.h. wenn er kleiner als der minimale oder größer als der maximale Wert ist
     */
    public int checkRange(int i) throws OutOfRangeException {
        if (i < min || i > max) throw new OutOfRangeException(i, this);
        return i;
    }

    /**
     * Ausnahme zum Anzeigen, das ein Wert außerhalb eines Bereiches liegt
     */
    public static final class OutOfRangeException extends IllegalRangeException {

        /**
         * Erzeugt eine neue {@link IllegalRangeException} mit entsprechender Nachricht
         *
         * @param i Der Wert, welcher Auslöser der Ausnahme ist
         * @param r Der Bereich, für den der Wert geprüft wurde
         */
        private OutOfRangeException(int i, Range r) {
            super("Value is out of range [%d,%d]: %d".formatted(r.min, r.max, i));
        }
    }

    /**
     * Ausnahme zum Anzeigen von unerlaubten Werten eines Bereiches
     */
    public static class IllegalRangeException extends RuntimeException {
        /**
         * Konstruktor; erzeugt eine neue Ausnahme mit entsprechender Nachricht
         *
         * @param min Der untere Wert des invaliden Bereichs
         * @param max Der oberere Wert des invaliden Bereichs
         */
        private IllegalRangeException(int min, int max) {
            super("Min value must not be greater than max value: %d, %d".formatted(min, max));
        }

        /**
         * vererbbarer Konstruktor; erzeugt eine neue Ausnahme rein von der übergebenen Nachricht
         *
         * @param message Die Nachricht, aus der die Ausnahme erzeugt werden soll
         */
        protected IllegalRangeException(String message) {
            super(message);
        }
    }
}
