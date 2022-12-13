package de.mi.common;

public record Subfield(int id, String name) {
    public Subfield {
        if (id < 0) throw new IllegalArgumentException("id must not be negativ");
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        return id == ((Subfield) o).id;
    }

    @Override
    public int hashCode() {
        return id;
    }

    @Override
    public String toString() {
        return "[" + id + "] " + name;
    }
}
