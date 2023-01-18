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

    public void subfield(Subfield subfield) throws IllegalArgumentException {
        if (subfield == null) throw new IllegalArgumentException("subfield id must not be negativ");
        subfields.add(subfield);
    }

    public BookFilterBuilder yearRange(int minYear, int maxYear) throws Range.IllegalRangeException {
        this.yearRange = new Range(
                Book.DEFAULT_YEAR_RANGE.checkRange(minYear),
                Book.DEFAULT_YEAR_RANGE.checkRange(maxYear)
        );
        return this;
    }

    public BookFilterBuilder pageRange(int minPages, int maxPages) throws Range.IllegalRangeException {
        pageRange = new Range(
                Book.DEFAULT_PAGE_RANGE.checkRange(minPages),
                Book.DEFAULT_PAGE_RANGE.checkRange(maxPages)
        );
        return this;
    }

    public BookFilterBuilder ratingRange(int minRating, int maxRating) throws Range.IllegalRangeException {
        ratingRange = new Range(
                Book.DEFAULT_RATING_RANGE.checkRange(minRating),
                Book.DEFAULT_RATING_RANGE.checkRange(maxRating)
        );
        return this;
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
                titleSearch,
                authorSearch
        );
    }
}
