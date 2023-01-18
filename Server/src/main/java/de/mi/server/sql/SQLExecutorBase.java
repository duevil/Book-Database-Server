package de.mi.server.sql;

import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Statement;

public abstract class SQLExecutorBase<T> implements SQLExecutor<T> {
    private final Statement statement;
    private final String sql;

    protected SQLExecutorBase(PreparedStatement statement) {
        this(statement, null);
    }

    protected SQLExecutorBase(Statement statement, String sql) {
        this.statement = statement;
        this.sql = sql;
    }

    Statement getStatement() {
        return statement;
    }

    String getSql() {
        return sql;
    }

    void setPreparedStatementValues(Object... values) throws SQLException {
        if (statement instanceof PreparedStatement preparedStatement) {
            for (int i = 0; i < values.length; i++) {
                preparedStatement.setObject(i + 1, values[i]);
            }
        } else throw new IllegalCallerException("executor does not use a prepared statement");
    }
}
