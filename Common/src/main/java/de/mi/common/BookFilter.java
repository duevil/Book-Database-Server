package de.mi.common;

import java.util.Optional;
import java.util.Set;

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

    public record Range(int min, int max) {
    }
}
