package de.mi.client.parser;

import java.util.Optional;

public interface ParseResult<T> {
    T get();

    T getOrThrow();

    default Optional<T> getOptional() {
        return Optional.ofNullable(get());
    }
}
