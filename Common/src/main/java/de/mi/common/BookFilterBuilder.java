package de.mi.common;

import java.util.Optional;
import java.util.Set;

public class BookFilterBuilder {
    private Set<Subfield> subfields;
    private BookFilter.Range yearRange;
    private BookFilter.Range pageRange;
    private String titleSearch;
    private String authorSearch;

    public BookFilterBuilder setSubfields(Set<Subfield> subfields) {
        this.subfields = subfields;
        return this;
    }

    public BookFilterBuilder setYearRange(int minYear, int maxYear) {
        this.yearRange = new BookFilter.Range(minYear, maxYear);
        return this;
    }

    public BookFilterBuilder setPageRange(int minPages, int maxPages) {
        this.pageRange = new BookFilter.Range(minPages, maxPages);
        return this;
    }

    public BookFilterBuilder setTitleSearch(String titleSearch) {
        this.titleSearch = titleSearch;
        return this;
    }

    public BookFilterBuilder setAuthorSearch(String authorSearch) {
        this.authorSearch = authorSearch;
        return this;
    }

    public BookFilter createBookFilter() {
        return new BookFilter(
                subfields,
                yearRange,
                pageRange,
                Optional.ofNullable(titleSearch),
                Optional.ofNullable(authorSearch)
        );
    }
}
