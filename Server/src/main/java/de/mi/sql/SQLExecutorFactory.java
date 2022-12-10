package de.mi.sql;

import de.mi.mapper.Mapper;

import java.nio.file.Path;
import java.sql.Statement;

public final class SQLExecutorFactory<T extends SQLExecutor<?>> {
    private final T executor;

    private SQLExecutorFactory(T executor) {
        this.executor = executor;
    }

    public static <V> SQLExecutorFactory<SQLQueryExecutor<V>> createQuery(Mapper<V> mapper) {
        return new SQLExecutorFactory<>(new SQLQueryExecutor<>(mapper));
    }

    public static SQLExecutorFactory<SQLUpdateExecutor> createUpdater() {
        return new SQLExecutorFactory<>(new SQLUpdateExecutor());
    }

    public static SQLExecutorFactory<SQLScriptRunner> createScriptRunner(Path scriptPath) {
        return new SQLExecutorFactory<>(new SQLScriptRunner(scriptPath));
    }

    public SQLExecutorFactory<T> setStatement(Statement statement) {
        executor.setStatement(statement);
        return this;
    }

    public SQLExecutorFactory<T> setSqlString(String sql) {
        executor.setSql(sql);
        return this;
    }

    public T get() throws IllegalStateException {
        if (executor.getStatement() == null)
            throw new IllegalStateException("the executor's statement was no yet initialized");
        return executor;
    }
}
