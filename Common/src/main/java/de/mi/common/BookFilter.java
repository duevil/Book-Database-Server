package de.mi.common;

import java.util.Optional;
import java.util.Set;

@SuppressWarnings("java:S1213")
public record BookFilter(
        Set<Integer> subfieldIDs,
        Range yearRange,
        Range pageRange,
        Optional<String> titleSearch,
        Optional<String> authorSearch
) {
    public static BookFilterBuilder builder() {
        return new BookFilterBuilder();
    }
}
