package de.mi.client.controller;

import de.mi.client.parser.PropertyParser;
import de.mi.common.Author;
import de.mi.common.Book;
import de.mi.common.Subfield;
import javafx.beans.property.IntegerProperty;
import javafx.beans.property.ListProperty;
import javafx.beans.property.SetProperty;
import javafx.beans.property.SimpleIntegerProperty;
import javafx.beans.property.SimpleListProperty;
import javafx.beans.property.SimpleSetProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.collections.FXCollections;

import java.util.List;
import java.util.Set;

class BookProperties {
    private final IntegerProperty idProperty
            = new SimpleIntegerProperty(this, "Book id");
    private final StringProperty titleProperty
            = new SimpleStringProperty(this, "Book title");
    private final ListProperty<AuthorProperties> authorsProperty
            = new SimpleListProperty<>(this, "Book authors", FXCollections.observableArrayList());
    private final StringProperty publisherProperty
            = new SimpleStringProperty(this, "Book publisher");
    private final StringProperty yearProperty
            = new SimpleStringProperty(this, "Book year");
    private final StringProperty pagesProperty
            = new SimpleStringProperty(this, "Book pages");
    private final IntegerProperty ratingProperty
            = new SimpleIntegerProperty(this, "Book rating");
    private final SetProperty<Subfield> subfieldsProperty
            = new SimpleSetProperty<>(this, "Book subfields", FXCollections.observableSet());

    public Book get() {
        int id = PropertyParser.parseInteger(idProperty).getOrThrow();
        String title = PropertyParser.parseString(titleProperty).getOrThrow();
        List<Author> authors = List.copyOf(authorsProperty.stream().map(AuthorProperties::get).toList());
        String publisher = PropertyParser.parseString(publisherProperty).getOrThrow();
        int year = PropertyParser.parseInteger(yearProperty, Book.DEFAULT_YEAR_RANGE).getOrThrow();
        int pages = PropertyParser.parseInteger(pagesProperty, Book.DEFAULT_PAGE_RANGE).getOrThrow();
        int rating = PropertyParser.parseInteger(ratingProperty, Book.DEFAULT_RATING_RANGE).getOrThrow();
        Set<Subfield> subfields = Set.copyOf(subfieldsProperty);
        if (subfields.isEmpty()) throw new IllegalStateException(subfieldsProperty.getName() + " is empty");

        return new Book(id, title, authors, publisher, year, pages, rating, subfields);
    }

    public void set(Book book) {
        idProperty.set(book.id());
        titleProperty.set(book.title());
        var authorElements = book.authors()
                .stream()
                .map(AuthorProperties::new)
                .toList();
        authorsProperty.clear();
        authorsProperty.addAll(authorElements);
        publisherProperty.set(book.publisher());
        yearProperty.set(String.valueOf(book.year()));
        pagesProperty.set(String.valueOf(book.pages()));
        ratingProperty.set(book.rating());
        subfieldsProperty.clear();
        subfieldsProperty.addAll(book.subfields());
    }

    public void clear() {
        idProperty.set(0);
        titleProperty.set(null);
        authorsProperty.clear();
        publisherProperty.set(null);
        yearProperty.set(null);
        pagesProperty.set(null);
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

    public StringProperty yearProperty() {
        return yearProperty;
    }

    public StringProperty pagesProperty() {
        return pagesProperty;
    }

    public SetProperty<Subfield> subfieldsProperty() {
        return subfieldsProperty;
    }
}
