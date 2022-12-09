package de.mi.sql;

import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Statement;

public final class SQLExecutorFactory<T extends SQLExecutor> {
    private final T executor;

    private SQLExecutorFactory(T executor) {
        this.executor = executor;
    }

    public static SQLExecutorFactory<SQLQueryExecutor> createQuery() {
        return new SQLExecutorFactory<>(new SQLQueryExecutor());
    }

    public static SQLExecutorFactory<SQLUpdateExecutor> createUpdater() {
        return new SQLExecutorFactory<>(new SQLUpdateExecutor());
    }

    public SQLExecutorFactory<T> forPreparedStatement(PreparedStatement statement) {
        executor.setStatement(statement);
        return this;
    }

    public SQLExecutorFactory<T> forStatement(Statement statement) throws SQLException {
        executor.setStatement(statement);
        return this;
    }

    public SQLExecutorFactory<T> forSqlString(String sql) {
        executor.setSql(sql);
        return this;
    }

    public T get() {
        return executor;
    }
}
