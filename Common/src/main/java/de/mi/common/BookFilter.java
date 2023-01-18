package de.mi.common;

import java.util.Set;

@SuppressWarnings("java:S1213")
public record BookFilter(
        Set<Subfield> subfields,
        Range yearRange,
        Range pageRange,
        Range ratingRange,
        String titleSearch,
        String authorSearch
) {
    public static BookFilterBuilder builder() {
        return new BookFilterBuilder();
    }
}
