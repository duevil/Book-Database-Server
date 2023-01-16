package de.mi.common;

public record Author(int id, String firstName, String lastName) {
    public Author {
        if (id < 0) throw new IllegalArgumentException("id must not be negativ");
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        return id == ((Author) o).id;
    }

    @Override
    public int hashCode() {
        return id;
    }

    @Override
    public String toString() {
        return firstName + ' ' + lastName;
    }
}
