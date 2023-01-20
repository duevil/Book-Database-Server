package de.mi.server.sql;

import java.io.InputStream;
import java.sql.PreparedStatement;
import java.sql.Statement;
import java.util.List;

public final class ExecutorFactory {

    private ExecutorFactory() {
    }

    public static <V> Executor<List<V>> createQuery(PreparedStatement statement, Mapper<V> mapper) {
        return new QueryExecutor<>(statement, mapper);
    }

    public static <V> Executor<List<V>> createQuery(Statement statement, String sql, Mapper<V> mapper) {
        return new QueryExecutor<>(statement, sql, mapper);
    }

    public static Executor<Integer> createUpdater(PreparedStatement statement) {
        return new UpdateExecutor(statement);
    }

    public static Executor<Void> createScriptRunner(Statement statement, InputStream data) {
        return new ScriptRunner(statement, data);
    }
}
