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
    public static final Range DEFAULT_YEAR_RANGE = new Range(1920, 2022);
    public static final Range DEFAULT_PAGE_RANGE = new Range(10, 500);

    public static BookFilterBuilder builder() {
        return new BookFilterBuilder();
    }

    public record Range(int min, int max) {
    }
}
