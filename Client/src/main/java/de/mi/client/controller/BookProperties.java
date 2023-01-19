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

    public StringProperty titleProperty() {
        return titleProperty;
    }

    public ListProperty<AuthorProperties> authorsProperty() {
        return authorsProperty;
    }

    public StringProperty publisherProperty() {
        return publisherProperty;
    }

    public Property<Integer> yearProperty() {
        return yearProperty;
    }

    public Property<Integer> pagesProperty() {
        return pagesProperty;
    }

    public IntegerProperty ratingProperty() {
        return ratingProperty;
    }

    public SetProperty<Subfield> subfieldsProperty() {
        return subfieldsProperty;
    }
}
