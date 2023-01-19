package de.mi.common;

public record Range(int min, int max) {
    public Range {
        if (min > max) throw new IllegalRangeException(min, max);
    }

    public int checkRange(int i) throws OutOfRangeException {
        if (i < min || i > max) throw new OutOfRangeException(i, this);
        return i;
    }

    public static class IllegalRangeException extends RuntimeException {
        private IllegalRangeException(int min, int max) {
            super("Min value must not be greater than max value: %d, %d".formatted(min, max));
        }

        protected IllegalRangeException(String message) {
            super(message);
        }
    }

    public static final class OutOfRangeException extends IllegalRangeException {

        private OutOfRangeException(int i, Range r) {
            super("Value is out of range [%d,%d]: %d".formatted(r.min, r.max, i));
        }
    }
}
