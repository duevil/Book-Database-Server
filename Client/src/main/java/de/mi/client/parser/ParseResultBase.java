package de.mi.client.parser;

import javafx.beans.property.ReadOnlyProperty;

abstract class ParseResultBase<T> implements ParseResult<T> {
    protected final ReadOnlyProperty<?> property;

    protected ParseResultBase(ReadOnlyProperty<?> property) {
        this.property = property;
    }

    protected abstract T get(boolean throwIfInvalid);

    @Override
    public T get() {
        return get(false);
    }

    @Override
    public T getOrThrow() {
        return get(true);
    }

    protected IllegalStateException createException(String reason, Throwable cause) {
        String message = property.getName() + ' ' + reason;
        return (cause == null)
                ? new IllegalStateException(message)
                : new IllegalStateException(message, cause);
    }
}
