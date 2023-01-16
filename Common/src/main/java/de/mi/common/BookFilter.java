package de.mi.common;

import java.util.Optional;
import java.util.Set;

@SuppressWarnings("java:S1213")
public record BookFilter(
        Set<Subfield> subfields,
        Range yearRange,
        Range pageRange,
        Optional<String> titleSearch,
        Optional<String> authorSearch
) {
    public static BookFilterBuilder builder() {
        return new BookFilterBuilder();
    }
}
