package de.mi.common;

import java.util.Set;

public record Book  (
            int id,
            String title,
            Set<Author> authors,
            String publisher,
            int year,
            int pages,
            Set<Subfield> subfields
) {

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
        throw new UnsupportedOperationException("use getter to create string");
    }
}
