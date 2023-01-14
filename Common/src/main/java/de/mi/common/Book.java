package de.mi.common;

import java.util.Set;

public record Book(
        int id,
        String title,
        Set<Author> authors,
        String publisher,
        int year,
        int pages,
        Set<Subfield> subfields
) {
    public static final Range DEFAULT_YEAR_RANGE = new Range(1920, 2023);
    public static final Range DEFAULT_PAGE_RANGE = new Range(10, 500);

    public Book {
        if (id < 0) throw new IllegalArgumentException("id must not be negativ");
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        return id == ((Book) o).id;
    }

    @Override
    public int hashCode() {
        return id;
    }

    @Override
    public String toString() {
        return String.format("%s (%d, %s)", title, year, publisher);
    }
}
