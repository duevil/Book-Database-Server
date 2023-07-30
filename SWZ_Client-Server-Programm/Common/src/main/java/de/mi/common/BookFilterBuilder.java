package de.mi.common;

import java.util.Collection;
import java.util.HashSet;

/**
 * Builder-Klasse zum schrittweisen Erzeugen eines {@link BookFilter Buchfilters}
 *
 * @author Malte Kasolowsky <code>m30114</code>
 */
public final class BookFilterBuilder {
    private final HashSet<Subfield> subfields = new HashSet<>();
    private Range yearRange = Book.DEFAULT_YEAR_RANGE;
    private Range pageRange = Book.DEFAULT_PAGE_RANGE;
    private Range ratingRange = Book.DEFAULT_RATING_RANGE;
    private String titleSearch;
    private String authorSearch;

    /**
     * Privater Konstruktor; eine Erzeugung einer Klassen-Instanz ist nicht nötig
     */
    BookFilterBuilder() {
    }

    /**
     * Speichert eine {@link Collection} mit zu filternden {@link Subfield Teilgebieten},
     * wobei vorher gespeicherte Teilgebiete entfernt werden
     * und die übergebene Collection in ein inneres {@link HashSet} übertragen werden
     *
     * @param subfields Die zu speichernde Collection
     * @return Die eigene Instanz
     * @throws IllegalArgumentException Wenn die angegebene Collection null ist
     */
    public BookFilterBuilder subfields(Collection<Subfield> subfields) throws IllegalArgumentException {
        if (subfields == null) throw new IllegalArgumentException("subfield collection must not be null");
        this.subfields.clear();
        subfields.forEach(this::subfield);
        return this;
    }

    /**
     * Speichert ein einzelnes zu filterndes {@link Subfield}
     *
     * @param subfield Das zu speichernde Teilgebiet
     * @throws IllegalArgumentException Wenn das angegebene Teilgebiet null ist
     */
    public void subfield(Subfield subfield) throws IllegalArgumentException {
        if (subfield == null) throw new IllegalArgumentException("subfield id must not be negativ");
        subfields.add(subfield);
    }

    /**
     * Speichert den zu filternden Jahres-Bereich
     *
     * @param minYear Der untere Wert des Bereiches
     * @param maxYear Der obere Wert des Bereiches
     * @throws Range.IllegalRangeException Wenn die Werte außerhalb von {@link Book#DEFAULT_YEAR_RANGE} liegen
     *                                     oder der untere Wert größer als der obere ist
     */
    public void yearRange(int minYear, int maxYear) throws Range.IllegalRangeException {
        this.yearRange = new Range(
                Book.DEFAULT_YEAR_RANGE.checkRange(minYear),
                Book.DEFAULT_YEAR_RANGE.checkRange(maxYear)
        );
    }

    /**
     * Speichert den zu filternden Seiten-Bereich
     *
     * @param minPages Der untere Wert des Bereiches
     * @param maxPages Der obere Wert des Bereiches
     * @throws Range.IllegalRangeException Wenn die Werte außerhalb von {@link Book#DEFAULT_PAGE_RANGE} liegen
     *                                     oder der untere Wert größer als der obere ist
     */
    public void pageRange(int minPages, int maxPages) throws Range.IllegalRangeException {
        pageRange = new Range(
                Book.DEFAULT_PAGE_RANGE.checkRange(minPages),
                Book.DEFAULT_PAGE_RANGE.checkRange(maxPages)
        );
    }

    /**
     * Speichert den zu filternden Bereich der Buchbewertung
     *
     * @param minRating Der untere Wert des Bereiches
     * @param maxRating Der obere Wert des Bereiches
     * @throws Range.IllegalRangeException Wenn die Werte außerhalb von {@link Book#DEFAULT_RATING_RANGE} liegen
     *                                     oder der untere Wert größer als der obere ist
     */
    public void ratingRange(int minRating, int maxRating) throws Range.IllegalRangeException {
        ratingRange = new Range(
                Book.DEFAULT_RATING_RANGE.checkRange(minRating),
                Book.DEFAULT_RATING_RANGE.checkRange(maxRating)
        );
    }

    /**
     * Speichert eine Zeichenkette um Suchen von Buchtiteln
     *
     * @param titleSearch Die zu suchende Zeichenkette
     * @return Die eigene Instanz
     */
    public BookFilterBuilder searchTitle(String titleSearch) {
        this.titleSearch = titleSearch;
        return this;
    }

    /**
     * Speichert eine Zeichenkette um Suchen von Autorenname
     *
     * @param authorSearch Die zu suchende Zeichenkette
     * @return Die eigene Instanz
     */
    public BookFilterBuilder searchAuthor(String authorSearch) {
        this.authorSearch = authorSearch;
        return this;
    }

    /**
     * Erzeugt einen neuen {@link BookFilter} aus den gespeicherten Daten
     *
     * @return Einen neuen Buchfilter
     */
    public BookFilter build() {
        return new BookFilter(
                subfields,
                yearRange,
                pageRange,
                ratingRange,
                titleSearch,
                authorSearch
        );
    }
}
