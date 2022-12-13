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
        return "[%d] %s (%d, %s)".formatted(id, title, year, publisher);
    }
}
