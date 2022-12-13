package de.mi.server;

import java.sql.SQLException;

@FunctionalInterface
interface ExConsumer<T> extends ExFunction<Void, T> {
    @Override
    default Void apply(T t) throws SQLException, IllegalArgumentException {
        accept(t);
        return null;
    }

    void accept(T t) throws SQLException, IllegalArgumentException;
}
