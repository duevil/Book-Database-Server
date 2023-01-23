package de.mi.server.rest;

import java.sql.SQLException;

@FunctionalInterface
interface ExSupplier<T> extends ExFunction<T, Void> {
    T get() throws SQLException, IllegalArgumentException;

    @Override
    default T apply(Void ignored) throws SQLException, IllegalArgumentException {
        return get();
    }
}
