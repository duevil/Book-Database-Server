package de.mi.common;

/**
 * Einfache Datenklasse zum Speichern und Übertragen von Autoren-Daten
 *
 * @param id        Die interne ID des Autors
 * @param firstName Der Vorname des Autors
 * @param lastName  Der Nachname des Autors
 * @author Malte Kasolowsky <code>m30114</code>
 */
public record Author(int id, String firstName, String lastName) {
    /**
     * record-Konstruktor;
     * prüft, ob die angegebene ID nicht negativ ist,
     * und wirft in diesem Fall eine {@link IllegalArgumentException}
     *
     * @throws IllegalArgumentException Wenn die angegebene Autor-ID negativ ist
     */
    public Author {
        if (id < 0) throw new IllegalArgumentException("id must not be negativ");
    }

    /**
     * Prüft, ob der Autor gleich zu einem anderen Objekt ist, sofern dieses ebenfalls ein Autor ist,
     * wobei zwei Autoren gleich sind, wenn und nur wenn beide IDs gleich sind
     *
     * @param o Das zu vergleichende Objekt
     * @return true, wenn beide Objekte dasselbe Objekt oder beide ein Buch sind und dieselbe ID haben
     */
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        return id == ((Author) o).id;
    }

    /**
     * Erzeugt einen HashCode für diesen Autor, welcher gleich der Autor-ID ist
     *
     * @return Der Wert der ID
     */
    @Override
    public int hashCode() {
        return id;
    }

    /**
     * Erzeugt eine Repräsentation des Autors als Zeichenkette,
     * wobei die Zeichenkette aus Vor- und Nachname des Autors besteht
     *
     * @return Vor- und Nachname des Autors mit einem Leerzeichen getrennt
     */
    @Override
    public String toString() {
        return firstName + ' ' + lastName;
    }
}
