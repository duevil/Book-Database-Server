package de.mi.sql;

import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Objects;

abstract class SQLExecutor<T> {
    private Statement statement;
    private String sql;

    SQLExecutor() {
    }

    public abstract T execute(Object... values) throws SQLException;

    public PreparedStatement getPreparedStatement() throws IllegalCallerException {
        if (statement instanceof PreparedStatement preparedStatement) return preparedStatement;
        else throw new IllegalCallerException("executor does not use a prepared statement");
    }

    Statement getStatement() {
        return statement;
    }

    void setStatement(Statement statement) {
        this.statement = Objects.requireNonNull(statement, "prepared statement");
    }

    String getSql() {
        return sql;
    }

    void setSql(String sql) {
        this.sql = Objects.requireNonNull(sql, "sql string");
    }

    void setPreparedStatementValues(Object... values) throws SQLException {
        PreparedStatement preparedStatement = this.getPreparedStatement();
        for (int i = 0; i < values.length; i++) {
            preparedStatement.setObject(i + 1, values[i]);
        }
    }
}
