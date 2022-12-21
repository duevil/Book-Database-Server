package de.mi.sql;

import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Statement;

public abstract class SQLExecutor<T> {
    private final Statement statement;
    private final String sql;

    protected SQLExecutor(PreparedStatement statement) {
        this(statement, null);
    }

    protected SQLExecutor(Statement statement, String sql) {
        this.statement = statement;
        this.sql = sql;
    }

    public abstract T execute(Object... values) throws SQLException;

    public PreparedStatement getPreparedStatement() throws IllegalCallerException {
        if (statement instanceof PreparedStatement preparedStatement) return preparedStatement;
        else throw new IllegalCallerException("executor does not use a prepared statement");
    }

    Statement getStatement() {
        return statement;
    }

    String getSql() {
        return sql;
    }

    void setPreparedStatementValues(Object... values) throws SQLException {
        PreparedStatement preparedStatement = this.getPreparedStatement();
        for (int i = 0; i < values.length; i++) {
            preparedStatement.setObject(i + 1, values[i]);
        }
    }
}
