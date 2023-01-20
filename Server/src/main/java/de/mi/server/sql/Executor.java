package de.mi.server.sql;

import java.sql.SQLException;

public interface Executor<T> {
    T execute(Object... values) throws SQLException;
}
