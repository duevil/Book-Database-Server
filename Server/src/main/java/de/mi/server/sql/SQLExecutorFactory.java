package de.mi.server.sql;

import de.mi.server.mapper.Mapper;

import java.io.InputStream;
import java.sql.PreparedStatement;
import java.sql.Statement;
import java.util.List;

public final class SQLExecutorFactory {

    private SQLExecutorFactory() {
    }

    public static <V> SQLExecutor<List<V>> createQuery(PreparedStatement statement, Mapper<V> mapper) {
        return new SQLQueryExecutor<>(statement, mapper);
    }

    public static <V> SQLExecutor<List<V>> createQuery(Statement statement, String sql, Mapper<V> mapper) {
        return new SQLQueryExecutor<>(statement, sql, mapper);
    }

    public static SQLExecutor<Integer> createUpdater(PreparedStatement statement) {
        return new SQLUpdateExecutor(statement);
    }

    public static SQLExecutor<Void> createScriptRunner(Statement statement, InputStream data) {
        return new SQLScriptRunner(statement, data);
    }
}
