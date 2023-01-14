package de.mi.common;

public record Range(int min, int max) {
    /**
     * @throws IllegalRangeException
     */
    public Range {
        if (min >= max) throw new IllegalRangeException(min, max);
    }

    public int checkRange(int i) throws OutOfRangeException {
        if (i < min || i > max) throw new OutOfRangeException(i, this);
        return i;
    }

    public static class IllegalRangeException extends RuntimeException {
        private IllegalRangeException(int min, int max) {
            super("Min must not be greater or equal to max: %d, %d".formatted(min, max));
        }

        protected IllegalRangeException(String message) {
            super(message);
        }
    }

    public static final class OutOfRangeException extends IllegalRangeException {
        private OutOfRangeException(int i, Range r) {
            super("Number out of range [%d,%d]: %d".formatted(r.min, r.max, i));
        }
    }
}
