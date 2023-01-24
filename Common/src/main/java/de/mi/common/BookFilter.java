package de.mi.common;

import java.util.Set;

/**
 * Einfache Datenklasse zum Speichern von Daten eines Buch-Filters
 *
 * @param subfields    Die {@link Subfield Teilgebiete}, für welche Bücher gefiltert werden sollen
 * @param yearRange    Der zu filternde Bereich des Bucherscheinungsjahres
 * @param pageRange    Der zu filternde Bereich der Anzahl an Buchseiten
 * @param ratingRange  Der zu filternde Bereich für die Bewertung eines Buches
 * @param titleSearch  Eine Zeichenkette zur Suche innerhalb der Buchtitel
 * @param authorSearch Eine Zeichenkette zur Suche innerhalb der Autorennamen
 * @author Malte Kasolowsky <code>m30114</code>
 */
public record BookFilter(
        Set<Subfield> subfields,
        Range yearRange,
        Range pageRange,
        Range ratingRange,
        String titleSearch,
        String authorSearch
) {
    /**
     * Factory-Methode zur Erzeugung eines neuen {@link BookFilterBuilder}
     * <p>
     * Die Erstellung einer Builder-Instanz ist nur hierüber möglich,
     * da beide Klassen eng miteinander verbunden sind und ein Builder alleine
     * wenig Zweck hat
     *
     * @return Eine neue Builder-Instanz
     */
    public static BookFilterBuilder builder() {
        return new BookFilterBuilder();
    }
}
