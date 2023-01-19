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

import java.util.function.BiConsumer;
import java.util.function.Consumer;

class FilterProperties {
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

    public void clear() {
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

    public StringProperty titleSearchProperty() {
        return titleSearchProperty;
    }

    public StringProperty authorSearchProperty() {
        return authorSearchProperty;
    }

    public Property<Integer> minYearProperty() {
        return yearRangeProperties.minProperty;
    }

    public Property<Integer> maxYearProperty() {
        return yearRangeProperties.maxProperty;
    }

    public Property<Integer> minPagesProperty() {
        return pageRangeProperties.minProperty;
    }

    public Property<Integer> maxPagesProperty() {
        return pageRangeProperties.maxProperty;
    }

    public IntegerProperty minRatingProperty() {
        return minRatingProperty;
    }

    public IntegerProperty maxRatingProperty() {
        return maxRatingProperty;
    }

    public SetProperty<Subfield> subfieldsProperty() {
        return subfieldsProperty;
    }

    private record RangeProperties(
            Range range,
            Property<Integer> minProperty,
            Property<Integer> maxProperty,
            String name
    ) implements Consumer<BiConsumer<Integer, Integer>> {

        @Override
        public void accept(BiConsumer<Integer, Integer> rangeConsumer) {
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
