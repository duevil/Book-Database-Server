package de.mi.common;

import java.util.List;
import java.util.Set;

/**
 * Einfache Datenklasse zum Speichern und Übertragen von Buch-Daten
 *
 * @param id        Die interne ID des Buches
 * @param title     Der Title des Buches
 * @param authors   Eine Liste mit Autoren für dieses Buch
 * @param publisher Der Verleger des Buches
 * @param year      Das Erscheinungsjahr des Buches
 * @param pages     Die Anzahl an Seiten, die das Buch hat
 * @param rating    Die Bewertung des Buches
 * @param subfields Die Teilgebiete des Buches
 * @author Malte Kasolowsky <code>m30114</code>
 */
public record Book(
        int id,
        String title,
        List<Author> authors,
        String publisher,
        Integer year,
        Integer pages,
        Integer rating,
        Set<Subfield> subfields
) {
    /**
     * Der erlaubte {@link Range Bereich} für das Buch-Jahr
     */
    public static final Range DEFAULT_YEAR_RANGE = new Range(1920, 2100);
    /**
     * Der erlaubte {@link Range Bereich} für die Buch-Seiten
     */
    public static final Range DEFAULT_PAGE_RANGE = new Range(10, Integer.MAX_VALUE);
    /**
     * Der erlaubte {@link Range Bereich} für die Buch-Bewertung
     */
    public static final Range DEFAULT_RATING_RANGE = new Range(1, 5);

    /**
     * record-Konstruktor;
     * prüft, ob die angegebene ID nicht negativ ist,
     * und wirft in diesem Fall eine {@link IllegalArgumentException}
     *
     * @throws IllegalArgumentException Wenn die angegebene Buch-ID negativ ist
     */
    public Book {
        if (id < 0) throw new IllegalArgumentException("id must not be negativ");
    }

    /**
     * Prüft, ob das Buch gleich zu einem anderen Objekt ist, sofern dieses ebenfalls ein Buch ist,
     * wobei zwei Bücher gleich sind, wenn und nur wenn beide IDs gleich sind
     *
     * @param o Das zu vergleichende Objekt
     * @return true, wenn beide Objekte dasselbe Objekt oder beide ein Buch sind und dieselbe ID haben
     */
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        return id == ((Book) o).id;
    }

    /**
     * Erzeugt einen HashCode für dieses Buch, welcher gleich der Buch-ID ist
     *
     * @return Der Wert der ID
     */
    @Override
    public int hashCode() {
        return id;
    }

    /**
     * Erzeugt eine Repräsentation des Buches als Zeichenkette,
     * wobei die Zeichenkette aus Title, Erscheinungsjahr, Verleger und ID des Buches besteht
     *
     * @return Eine Zeichenkette mit Title, Erscheinungsjahr, Verleger und ID des Buches
     */
    @Override
    public String toString() {
        return String.format("%s (%d, %s) [%d]", title, year, publisher, id);
    }
}
