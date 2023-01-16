package de.mi.common;

import java.util.Collection;
import java.util.HashSet;
import java.util.Optional;

@SuppressWarnings("java:S109")
public final class BookFilterBuilder {
    private final HashSet<Subfield> subfields = new HashSet<>();
    private Range yearRange = Book.DEFAULT_YEAR_RANGE;
    private Range pageRange = Book.DEFAULT_PAGE_RANGE;
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
        if (subfield == null) {
            throw new IllegalArgumentException("subfield id must not be negativ");
        }
        subfields.add(subfield);
        return this;
    }

    public BookFilterBuilder removeSubfield(Subfield subfield) {
        if (subfield == null) {
            throw new IllegalArgumentException("subfield id must not be negativ");
        }
        subfields.remove(subfield);
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

    public BookFilterBuilder maxPages(int maxPages) {
        return pageRange(pageRange.min(), maxPages);
    }

    public BookFilterBuilder minPages(int minPages) {
        return pageRange(minPages, pageRange.max());
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
                Optional.ofNullable(titleSearch),
                Optional.ofNullable(authorSearch)
        );
    }
}
