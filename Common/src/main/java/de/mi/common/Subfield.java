package de.mi.common;

/**
 * Einfache Datenklassen zum Speichern und Übertragen von Daten zu Teilgebieten
 * <p>
 * Die Klasse implementiert das {@link Comparable}-Interface,
 * um die ungestörte Speicherung von Teilgebieten als Schlüssel in einer {@link java.util.HashMap} zu erlauben
 *
 * @param id   Die interne ID des Teilgebietes
 * @param name Der Name des Teilgebietes
 * @author Malte Kasolowsky <code>m30114</code>
 */
public record Subfield(int id, String name) implements Comparable<Subfield> {
    /**
     * record-Konstruktor;
     * prüft, ob die angegebene ID nicht negativ ist,
     * und wirft in diesem Fall eine {@link IllegalArgumentException}
     *
     * @throws IllegalArgumentException Wenn die angegebene Teilgebiet-ID negativ ist
     */
    public Subfield {
        if (id < 0) throw new IllegalArgumentException("id must not be negativ");
    }

    /**
     * Prüft, ob das Teilgebiet gleich zu einem anderen Objekt ist, sofern dieses ebenfalls ein Teilgebiet ist,
     * wobei zwei Teilgebiete gleich sind, wenn und nur wenn beide IDs gleich sind
     *
     * @param o Das zu vergleichende Objekt
     * @return true, wenn beide Objekte dasselbe Objekt oder beide ein Teilgebiet sind und dieselbe ID haben
     */
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        return id == ((Subfield) o).id;
    }

    /**
     * Erzeugt einen HashCode für dieses Teilgebiet, welcher gleich der Teilgebiet-ID ist
     *
     * @return Der Wert der ID
     */
    @Override
    public int hashCode() {
        return id;
    }


    /**
     * Erzeugt eine Repräsentation des Teilgebietes als Zeichenkette,
     * wobei die Zeichenkette aus dem Namen und der ID des Teilgebietes besteht
     *
     * @return Der Name und die ID des Teilgebietes
     */
    @Override
    public String toString() {
        return String.format("%s [%d]", name, id);
    }

    /**
     * Vergleicht dieses Teilgebiet mit einem weiteren,
     * wobei beide Teilgebiete anhand ihrer ID verglichen werden
     *
     * @param subfield Das zu vergleichende Teilgebiet
     * @return Das Resultat vom Vergleich beider IDs mittels {@link Integer#compare(int, int)}
     */
    @Override
    public int compareTo(Subfield subfield) {
        return Integer.compare(id, subfield.id);
    }
}
