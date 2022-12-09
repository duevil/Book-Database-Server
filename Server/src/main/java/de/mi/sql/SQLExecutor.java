package de.mi.sql;

import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Objects;

abstract class SQLExecutor {
    private Statement statement;
    private String sql;

    SQLExecutor() {
    }

    public abstract void execute() throws SQLException;

    public PreparedStatement getPreparedStatement() {
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
}
