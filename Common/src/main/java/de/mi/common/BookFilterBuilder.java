package de.mi.common;

import java.util.Collection;
import java.util.HashSet;
import java.util.Optional;

@SuppressWarnings("java:S109")
public class BookFilterBuilder {
    private final HashSet<Subfield> subfields = new HashSet<>();
    private BookFilter.Range yearRange = new BookFilter.Range(1920, 2022);
    private BookFilter.Range pageRange = new BookFilter.Range(10, 500);
    private String titleSearch;
    private String authorSearch;

    public BookFilterBuilder subfields(Collection<Subfield> subfields) {
        this.subfields.addAll(subfields);
        return this;
    }

    public BookFilterBuilder subfield(Subfield subfield) {
        this.subfields.add(subfield);
        return this;
    }

    public BookFilterBuilder yearRange(int minYear, int maxYear) {
        if (!(minYear >= 1900 && minYear < maxYear)) {
            throw new IllegalArgumentException("range mus be positive and min must be less than max");
        }
        this.yearRange = new BookFilter.Range(minYear, maxYear);
        return this;
    }

    public BookFilterBuilder pageRange(int minPages, int maxPages) {
        if (!(minPages > 0 && minPages < maxPages)) {
            throw new IllegalArgumentException("range mus be positive and min must be less than max");
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
                subfields,
                yearRange,
                pageRange,
                Optional.ofNullable(titleSearch),
                Optional.ofNullable(authorSearch)
        );
    }
}
