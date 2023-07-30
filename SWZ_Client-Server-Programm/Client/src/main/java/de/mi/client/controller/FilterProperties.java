package de.mi.client.controller;

import de.mi.common.Book;
import de.mi.common.BookFilter;
import de.mi.common.BookFilterBuilder;
import de.mi.common.Range;
import de.mi.common.Subfield;
import javafx.beans.property.IntegerProperty;
import javafx.beans.property.Property;
import javafx.beans.property.SetProperty;
import javafx.beans.property.SimpleIntegerProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleSetProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.collections.FXCollections;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.function.BiConsumer;
import java.util.function.Consumer;

/**
 * Klasse, welche die Werte eines {@link BookFilter Buchflters}
 * mittels {@link Property Properties} darstellt
 *
 * @author Malte Kasolowsky <code>m30114</code>
 */
class FilterProperties {
    private static final Logger LOGGER = LogManager.getLogger(FilterProperties.class);
    private final StringProperty titleSearchProperty
            = new SimpleStringProperty(this, "Filter title search");
    private final StringProperty authorSearchProperty
            = new SimpleStringProperty(this, "Filter author search");
    private final RangeProperties yearRangeProperties
            = new RangeProperties(Book.DEFAULT_YEAR_RANGE,
            new SimpleObjectProperty<>(this, "Filter year range min"),
            new SimpleObjectProperty<>(this, "Filter year range max"),
            "Filter year range");
    private final RangeProperties pageRangeProperties
            = new RangeProperties(Book.DEFAULT_PAGE_RANGE,
            new SimpleObjectProperty<>(this, "Filter page range min"),
            new SimpleObjectProperty<>(this, "Filter page range max"),
            "Filter page range");
    private final IntegerProperty minRatingProperty
            = new SimpleIntegerProperty(this, "Filter rating range min");
    private final IntegerProperty maxRatingProperty
            = new SimpleIntegerProperty(this, "Filter rating range max");
    private final RangeProperties ratingRangeProperties
            = new RangeProperties(Book.DEFAULT_RATING_RANGE,
            minRatingProperty.asObject(),
            maxRatingProperty.asObject(),
            "Filter rating range");
    private final SetProperty<Subfield> subfieldsProperty
            = new SimpleSetProperty<>(this, "Filter subfields", FXCollections.observableSet());

    /**
     * Liest die aktuellen Werte der unterliegenden {@link Property Properties} aus
     * und erzeugt daraus einen neuen {@link BookFilter}; sind die Werte null oder leer,
     * so werden diese trotzdem akzeptiert oder mit Standardwerten ersetzt
     *
     * @return Einen neuen Filter mit den Werten der Properties
     * @throws Util.PropertyException Wenn einer der {@link Range}-Werte des Filters
     *                                außerhalb ihres erlaubten Bereichs liegen
     */
    public BookFilter get() throws Util.PropertyException {
        BookFilterBuilder builder = BookFilter.builder()
                .searchTitle(titleSearchProperty.get())
                .searchAuthor(authorSearchProperty.get())
                .subfields(subfieldsProperty);
        yearRangeProperties.accept(builder::yearRange);
        pageRangeProperties.accept(builder::pageRange);
        ratingRangeProperties.accept(builder::ratingRange);
        return builder.build();
    }

    /**
     * Setzt die aktuell eingegebenen Werte des Filters zurück
     */
    public void clear() {
        LOGGER.debug("Clearing filter");

        titleSearchProperty.set(null);
        authorSearchProperty.set(null);
        yearRangeProperties.minProperty.setValue(null);
        yearRangeProperties.maxProperty.setValue(null);
        pageRangeProperties.minProperty.setValue(null);
        pageRangeProperties.maxProperty.setValue(null);
        minRatingProperty.set(Book.DEFAULT_RATING_RANGE.min());
        maxRatingProperty.set(Book.DEFAULT_RATING_RANGE.max());
        subfieldsProperty.clear();
    }

    /**
     * Getter für den Wert der {@link BookFilter}-Titelsuche
     *
     * @return Die {@link Property}, welches die Titelsuche des Filters enthält
     */
    public StringProperty titleSearchProperty() {
        return titleSearchProperty;
    }

    /**
     * Getter für den Wert der {@link BookFilter}-Autor-Namenssuche
     *
     * @return Die {@link Property}, welches die Titel-Suche des Filters enthält
     */
    public StringProperty authorSearchProperty() {
        return authorSearchProperty;
    }

    /**
     * Getter für den unteren Wert des {@link BookFilter}-Jahresbereichs
     *
     * @return Die {@link Property}, welches die untere Grenze des Jahresbereichs des Filters enthält
     */
    public Property<Integer> minYearProperty() {
        return yearRangeProperties.minProperty;
    }

    /**
     * Getter für den oberen Wert des {@link BookFilter}-Jahresbereichs
     *
     * @return Die {@link Property}, welches die obere Grenze des Jahresbereichs des Filters enthält
     */
    public Property<Integer> maxYearProperty() {
        return yearRangeProperties.maxProperty;
    }

    /**
     * Getter für den unteren Wert des {@link BookFilter}-Seitenbereichs
     *
     * @return Die {@link Property}, welches die untere Grenze des Seitenbereichs des Filters enthält
     */
    public Property<Integer> minPagesProperty() {
        return pageRangeProperties.minProperty;
    }

    /**
     * Getter für den oberen Wert des {@link BookFilter}-Seitenbereichs
     *
     * @return Die {@link Property}, welches die obere Grenze des Seitenbereichs des Filters enthält
     */
    public Property<Integer> maxPagesProperty() {
        return pageRangeProperties.maxProperty;
    }

    /**
     * Getter für den unteren Wert des {@link BookFilter}-Bewertungsbereichs
     *
     * @return Die {@link Property}, welches die untere Grenze des Bewertungsbereichs des Filters enthält
     */
    public IntegerProperty minRatingProperty() {
        return minRatingProperty;
    }

    /**
     * Getter für den oberen Wert des {@link BookFilter}-Bewertungsbereichs
     *
     * @return Die {@link Property}, welches die obere Grenze des Bewertungsbereichs des Filters enthält
     */
    public IntegerProperty maxRatingProperty() {
        return maxRatingProperty;
    }

    /**
     * Getter für den Wert der {@link BookFilter}-Teilgebiete
     *
     * @return Die {@link SetProperty}, welches die Teilgebiete des Filters enthält
     */
    public SetProperty<Subfield> subfieldsProperty() {
        return subfieldsProperty;
    }

    /**
     * Einfache Datenklasse zum einfachen Verwalten von {@link Property Properties}
     * innerhalb einer gegebenen {@link Range}
     *
     * @param range       Der Bereich, für dem die untere und obere Grenze gespeichert werden soll
     * @param minProperty Die Property, welche die untere Grenze eines Bereichs innerhalb des gegebenen enthält
     * @param maxProperty Die Property, welche die obere Grenze eines Bereichs innerhalb des gegebenen enthält
     * @param name        Der Name der Instanz dieser Klasse
     */
    private record RangeProperties(
            Range range,
            Property<Integer> minProperty,
            Property<Integer> maxProperty,
            String name
    ) implements Consumer<BiConsumer<Integer, Integer>> {

        /**
         * Gibt die aktuellen Werte der gespeicherten {@link Property Properties}
         * oder den minimalen bzw. maximalen Wert des ursprünglichen Bereiches,
         * sollte einer Properties außerhalb dieses Bereichs liegen,
         * an den gegeben {@link BiConsumer} weiter
         *
         * @param rangeConsumer Ein Ausdruck, welcher die beiden Bereichsgrenzen übergeben bekommt
         * @throws Util.PropertyException Wenn der Werte der Property für die untere Grenze
         *                                größer als der für die obere Grenze ist
         */
        @Override
        public void accept(BiConsumer<Integer, Integer> rangeConsumer) throws Util.PropertyException {
            try {
                rangeConsumer.accept(
                        Util.readPropertyOptional(minProperty, range).orMin(),
                        Util.readPropertyOptional(maxProperty, range).orMax());
            } catch (Range.IllegalRangeException e) {
                throw Util.createPropertyException(name, e);
            }
        }
    }
}
