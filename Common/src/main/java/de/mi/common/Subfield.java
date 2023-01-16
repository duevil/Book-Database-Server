package de.mi.common;

import java.util.Objects;

public record Subfield(String name) implements Comparable<Subfield> {

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        return Objects.equals(this.name, ((Subfield) o).name);
    }

    @Override
    public int hashCode() {
        return Objects.hash(name);
    }

    @Override
    public String toString() {
        return name;
    }

    @Override
    public int compareTo(Subfield subfield) {
        return Objects.compare(this.name, subfield.name, String::compareTo);
    }
}
