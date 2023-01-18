package de.mi.server.sql;

import java.sql.SQLException;

public interface SQLExecutor<T> {
    T execute(Object... values) throws SQLException;
}
