package de.mi.server.rest;

import java.sql.SQLException;

@FunctionalInterface
interface ExFunction<R, T> {
    R apply(T t) throws SQLException, IllegalArgumentException;
}
