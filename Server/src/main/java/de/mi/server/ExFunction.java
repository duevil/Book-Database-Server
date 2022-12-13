package de.mi.server;

import java.sql.SQLException;

@FunctionalInterface
interface ExFunction<R, T> {
    R apply(T t) throws SQLException, IllegalArgumentException;
}
