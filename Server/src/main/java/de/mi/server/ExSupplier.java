package de.mi.server;

import java.sql.SQLException;

@FunctionalInterface
interface ExSupplier<T> extends ExFunction<T, Void> {
    @Override
    default T apply(Void ignored) throws SQLException, IllegalArgumentException {
        return get();
    }

    T get() throws SQLException, IllegalArgumentException;
}
