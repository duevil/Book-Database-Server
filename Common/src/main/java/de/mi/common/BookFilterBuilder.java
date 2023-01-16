package de.mi.common;

import java.util.Collection;
import java.util.HashSet;
import java.util.Optional;

@SuppressWarnings("java:S109")
public final class BookFilterBuilder {
    private final HashSet<Subfield> subfields = new HashSet<>();
    private Range yearRange = Book.DEFAULT_YEAR_RANGE;
    private Range pageRange = Book.DEFAULT_PAGE_RANGE;
    private Range ratingRange = Book.DEFAULT_RATING_RANGE;
    private String titleSearch;
    private String authorSearch;

    BookFilterBuilder() {
    }

    public BookFilterBuilder subfields(Collection<Subfield> subfields) {
        this.subfields.clear();
        Optional.ofNullable(subfields).orElseThrow(
                () -> new IllegalArgumentException("subfield collection must not be null")
        ).forEach(this::subfield);
        return this;
    }

    public BookFilterBuilder subfield(Subfield subfield) throws IllegalArgumentException {
        subfields.add(Optional.ofNullable(subfield).orElseThrow(
                () -> new IllegalArgumentException("subfield must not be null")
        ));
        return this;
    }

    public BookFilterBuilder removeSubfield(Subfield subfield) {
        subfields.remove(Optional.ofNullable(subfield).orElseThrow(
                () -> new IllegalArgumentException("subfield must not be null")
        ));
        return this;
    }

    public BookFilterBuilder yearRange(Range yearRange) {
        this.yearRange = Optional.ofNullable(yearRange).orElseThrow(
                () -> new IllegalArgumentException("year range must not be null")
        );
        return this;
    }

    public BookFilterBuilder yearRange(int minYear, int maxYear) throws Range.IllegalRangeException {
        return yearRange(new Range(
                Book.DEFAULT_YEAR_RANGE.checkRange(minYear),
                Book.DEFAULT_YEAR_RANGE.checkRange(maxYear)
        ));
    }

    public BookFilterBuilder minYear(int minYear) {
        return yearRange(minYear, yearRange.max());
    }

    public BookFilterBuilder maxYear(int maxYear) {
        return yearRange(yearRange.min(), maxYear);
    }

    public BookFilterBuilder pageRange(Range pageRange) {
        this.pageRange = Optional.ofNullable(pageRange).orElseThrow(
                () -> new IllegalArgumentException("page range must not be null")
        );
        return this;
    }

    public BookFilterBuilder pageRange(int minPages, int maxPages) throws Range.IllegalRangeException {
        return pageRange(new Range(
                Book.DEFAULT_PAGE_RANGE.checkRange(minPages),
                Book.DEFAULT_PAGE_RANGE.checkRange(maxPages)
        ));
    }

    public BookFilterBuilder minPages(int minPages) {
        return pageRange(minPages, pageRange.max());
    }

    public BookFilterBuilder maxPages(int maxPages) {
        return pageRange(pageRange.min(), maxPages);
    }

    public BookFilterBuilder ratingRange(Range ratingRange) {
        this.ratingRange = Optional.ofNullable(ratingRange).orElseThrow(
                () -> new IllegalArgumentException("rating range must not be null")
        );
        return this;
    }

    public BookFilterBuilder ratingRange(int minRating, int maxRating) throws Range.IllegalRangeException {
        return ratingRange(new Range(
                Book.DEFAULT_RATING_RANGE.checkRange(minRating),
                Book.DEFAULT_RATING_RANGE.checkRange(maxRating)
        ));
    }

    public BookFilterBuilder minRating(int minRating) {
        return ratingRange(minRating, ratingRange.max());
    }

    public BookFilterBuilder maxRating(int maxRating) {
        return ratingRange(ratingRange.min(), maxRating);
    }

    public BookFilterBuilder searchTitle(String titleSearch) {
        this.titleSearch = titleSearch;
        return this;
    }

    public BookFilterBuilder searchAuthor(String authorSearch) {
        this.authorSearch = authorSearch;
        return this;
    }

    public BookFilter build() {
        return new BookFilter(
                subfields,
                yearRange,
                pageRange,
                ratingRange,
                Optional.ofNullable(titleSearch),
                Optional.ofNullable(authorSearch)
        );
    }
}
