package de.mi.sql;

import de.mi.mapper.Mapper;

import java.io.InputStream;
import java.sql.PreparedStatement;
import java.sql.Statement;

public final class SQLExecutorFactory {

    private SQLExecutorFactory() {
    }

    public static <V> SQLQueryExecutor<V> createQuery(PreparedStatement statement, Mapper<V> mapper) {
        return new SQLQueryExecutor<>(statement, mapper);
    }

    public static <V> SQLQueryExecutor<V> createQuery(Statement statement, String sql, Mapper<V> mapper) {
        return new SQLQueryExecutor<>(statement, sql, mapper);
    }

    public static SQLUpdateExecutor createUpdater(PreparedStatement statement) {
        return new SQLUpdateExecutor(statement);
    }

    public static SQLScriptRunner createScriptRunner(Statement statement, InputStream data) {
        return new SQLScriptRunner(statement, data);
    }
}
