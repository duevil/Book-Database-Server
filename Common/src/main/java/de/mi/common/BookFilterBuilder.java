package de.mi.common;

import java.util.Collection;
import java.util.HashSet;
import java.util.Optional;

@SuppressWarnings("java:S109")
public final class BookFilterBuilder {
    private final HashSet<Integer> subfieldIDs = new HashSet<>();
    private BookFilter.Range yearRange = BookFilter.DEFAULT_YEAR_RANGE;
    private BookFilter.Range pageRange = BookFilter.DEFAULT_PAGE_RANGE;
    private String titleSearch;
    private String authorSearch;

    BookFilterBuilder() {
    }

    public BookFilterBuilder subfields(Collection<Integer> subfieldIDs) {
        if (subfieldIDs != null) this.subfieldIDs.addAll(subfieldIDs);
        return this;
    }

    public BookFilterBuilder subfield(int subfieldID) {
        this.subfieldIDs.add(subfieldID);
        return this;
    }

    public BookFilterBuilder yearRange(int minYear, int maxYear) {
        if (!(minYear >= 1900 && minYear < maxYear)) {
            throw new IllegalArgumentException(String.format(
                    "range must be positive and min must be less than max: %d, %d",
                    minYear,
                    maxYear
            ));
        }
        this.yearRange = new BookFilter.Range(minYear, maxYear);
        return this;
    }

    public BookFilterBuilder pageRange(int minPages, int maxPages) {
        if (!(minPages > 0 && minPages < maxPages)) {
            throw new IllegalArgumentException(String.format(
                    "range must be positive and min must be less than max: %d, %d",
                    minPages,
                    maxPages
            ));
        }
        this.pageRange = new BookFilter.Range(minPages, maxPages);
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
                subfieldIDs,
                yearRange,
                pageRange,
                Optional.ofNullable(titleSearch),
                Optional.ofNullable(authorSearch)
        );
    }
}
