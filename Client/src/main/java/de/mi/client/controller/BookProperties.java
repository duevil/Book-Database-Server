package de.mi.client.controller;

import de.mi.common.Author;
import de.mi.common.Book;
import de.mi.common.Subfield;
import javafx.beans.property.IntegerProperty;
import javafx.beans.property.ListProperty;
import javafx.beans.property.Property;
import javafx.beans.property.SetProperty;
import javafx.beans.property.SimpleIntegerProperty;
import javafx.beans.property.SimpleListProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleSetProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.collections.FXCollections;

import java.util.List;
import java.util.Set;

/**
 * Klasse, welche die Werte eines {@link Book Buches}
 * mittels {@link javafx.beans.property.Property Properties} darstellt
 *
 * @author Malte Kasolowsky <code>m30114</code>
 */
class BookProperties {
    private final StringProperty titleProperty
            = new SimpleStringProperty(this, "Book title");
    private final ListProperty<AuthorProperties> authorsProperty
            = new SimpleListProperty<>(this, "Book authors", FXCollections.observableArrayList());
    private final StringProperty publisherProperty
            = new SimpleStringProperty(this, "Book publisher");
    private final Property<Integer> yearProperty
            = new SimpleObjectProperty<>(this, "Book year");
    private final Property<Integer> pagesProperty
            = new SimpleObjectProperty<>(this, "Book pages");
    private final IntegerProperty ratingProperty
            = new SimpleIntegerProperty(this, "Book rating");
    private final SetProperty<Subfield> subfieldsProperty
            = new SimpleSetProperty<>(this, "Book subfields", FXCollections.observableSet());
    private int id = -1;

    /**
     * Liest aktuellen Werte die unterliegenden {@link Property Properties} aus
     * und erzeugt daraus ein neues {@link Book}
     *
     * @return Einen neues Buch mit den Werten der Properties und der gespeicherten ID
     * @throws Util.PropertyException Falls die Werte leer oder bei den Zahlen außerhalb des Standardbereiches liegen
     */
    public Book get() throws Util.PropertyException {

        String title = Util.readProperty(titleProperty);
        List<Author> authors = Util.readPropertyOptional(authorsProperty)
                .filter(set -> !set.isEmpty())
                .orElseThrow(Util.emptyPropertyExceptionSupplier(authorsProperty))
                .stream()
                .map(AuthorProperties::get)
                .toList();
        String publisher = Util.readProperty(publisherProperty);
        Integer year = Util.readProperty(yearProperty, Book.DEFAULT_YEAR_RANGE);
        Integer pages = Util.readProperty(pagesProperty, Book.DEFAULT_PAGE_RANGE);
        int rating = Util.readProperty(ratingProperty.asObject(), Book.DEFAULT_RATING_RANGE);
        Set<Subfield> subfields = Util.readPropertyOptional(subfieldsProperty)
                .filter(set -> !set.isEmpty())
                .orElseThrow(Util.emptyPropertyExceptionSupplier(subfieldsProperty));

        return new Book(id, title, authors, publisher, year, pages, rating, subfields);
    }

    /**
     * Setzt die Werte der unterliegenden {@link Property Properties}
     * auf die Werte des übergebenen {@link Book Buches}
     * und speichert die ID des Buches
     *
     * @param book Das Buch, dessen Werte ausgelesen werden soll
     */
    public void set(Book book) {
        id = book.id();
        titleProperty.set(book.title());
        var authorElements = book.authors()
                .stream()
                .map(AuthorProperties::new)
                .toList();
        authorsProperty.clear();
        authorsProperty.addAll(authorElements);
        publisherProperty.set(book.publisher());
        yearProperty.setValue(book.year());
        pagesProperty.setValue(book.pages());
        ratingProperty.setValue(book.rating());
        subfieldsProperty.clear();
        subfieldsProperty.addAll(book.subfields());
    }

    /**
     * Setzt die Werte der unterliegenden {@link Property Properties} auf null
     * bzw. leert die Collection-Properties und setzt die gespeicherte Buch-ID auf 0
     */
    public void clear() {
        id = 0;
        titleProperty.set(null);
        authorsProperty.clear();
        publisherProperty.set(null);
        yearProperty.setValue(null);
        pagesProperty.setValue(null);
        ratingProperty.setValue(null);
        subfieldsProperty.clear();
    }

    /**
     * Getter für den {@link Book}-Title
     *
     * @return Die {@link Property}, welche den Titel des Buches enthält
     */
    public StringProperty titleProperty() {
        return titleProperty;
    }


    /**
     * Getter für die {@link Book}-{@link AuthorProperties Autoren}
     *
     * @return Die {@link ListProperty}, welche die Autoren des Buches enthält
     */
    public ListProperty<AuthorProperties> authorsProperty() {
        return authorsProperty;
    }


    /**
     * Getter für den {@link Book}-Verleger
     *
     * @return Die {@link Property}, welche den Verleger des Buches enthält
     */
    public StringProperty publisherProperty() {
        return publisherProperty;
    }


    /**
     * Getter für das {@link Book}-Erscheinungsjahr
     *
     * @return Die {@link Property}, welche das Erscheinungsjahr des Buches enthält
     */
    public Property<Integer> yearProperty() {
        return yearProperty;
    }


    /**
     * Getter für die {@link Book}-Seitenzahl
     *
     * @return Die {@link Property}, welche die Anzahl an Seiten des Buches enthält
     */
    public Property<Integer> pagesProperty() {
        return pagesProperty;
    }


    /**
     * Getter für die {@link Book}-Bewertung
     *
     * @return Die {@link Property}, welche die Bewertung des Buches enthält
     */
    public IntegerProperty ratingProperty() {
        return ratingProperty;
    }


    /**
     * Getter für die {@link Book}-{@link Subfield Teilgebiete}
     *
     * @return Die {@link SetProperty}, welche die Teilgebiete des Buches enthält
     */
    public SetProperty<Subfield> subfieldsProperty() {
        return subfieldsProperty;
    }
}
