package de.mi.client.controller;

import de.mi.client.parser.ParseResult;
import de.mi.client.parser.PropertyParser;
import de.mi.common.Book;
import de.mi.common.BookFilter;
import de.mi.common.BookFilterBuilder;
import de.mi.common.Subfield;
import javafx.beans.property.SetProperty;
import javafx.beans.property.SimpleSetProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.collections.FXCollections;

class FilterProperties {
    private final StringProperty titleSearchProperty
            = new SimpleStringProperty(this, "Filter title search");
    private final StringProperty authorSearchProperty
            = new SimpleStringProperty(this, "Filter author search");
    private final StringProperty minYearProperty
            = new SimpleStringProperty(this, "Filter year range min");
    private final StringProperty maxYearProperty
            = new SimpleStringProperty(this, "Filter year range max");
    private final StringProperty minPagesProperty
            = new SimpleStringProperty(this, "Filter page range min");
    private final StringProperty maxPagesProperty
            = new SimpleStringProperty(this, "Filter page range max");
    private final SetProperty<Subfield> subfields
            = new SimpleSetProperty<>(this, "Filter subfields", FXCollections.observableSet());

    public BookFilter getFilter() throws IllegalStateException {
        ParseResult<String> strResult;
        ParseResult<Integer> intResult;
        BookFilterBuilder builder = BookFilter.builder();

        strResult = PropertyParser.parseString(titleSearchProperty);
        strResult.getOptional().ifPresent(builder::searchTitle);

        strResult = PropertyParser.parseString(authorSearchProperty);
        strResult.getOptional().ifPresent(builder::searchAuthor);

        intResult = PropertyParser.parseInteger(minYearProperty, Book.DEFAULT_YEAR_RANGE);
        intResult.getOptional().ifPresent(builder::minYear);

        intResult = PropertyParser.parseInteger(maxYearProperty, Book.DEFAULT_YEAR_RANGE);
        intResult.getOptional().ifPresent(builder::maxYear);

        intResult = PropertyParser.parseInteger(minPagesProperty, Book.DEFAULT_PAGE_RANGE);
        intResult.getOptional().ifPresent(builder::minPages);

        intResult = PropertyParser.parseInteger(maxPagesProperty, Book.DEFAULT_PAGE_RANGE);
        intResult.getOptional().ifPresent(builder::maxPages);

        return builder.subfields(subfields).build();
    }

    public StringProperty titleSearchProperty() {
        return titleSearchProperty;
    }

    public StringProperty authorSearchProperty() {
        return authorSearchProperty;
    }

    public StringProperty minYearProperty() {
        return minYearProperty;
    }

    public StringProperty maxYearProperty() {
        return maxYearProperty;
    }

    public StringProperty minPagesProperty() {
        return minPagesProperty;
    }

    public StringProperty maxPagesProperty() {
        return maxPagesProperty;
    }

    public SetProperty<Subfield> subfieldsProperty() {
        return subfields;
    }
}
