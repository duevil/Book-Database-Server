package de.mi.server.rest;

import java.sql.SQLException;

@FunctionalInterface
interface ExConsumer<T> extends ExFunction<Void, T> {
    void accept(T t) throws SQLException, IllegalArgumentException;

    @Override
    default Void apply(T t) throws SQLException, IllegalArgumentException {
        accept(t);
        return null;
    }
}
