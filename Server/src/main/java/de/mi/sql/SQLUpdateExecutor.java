package de.mi.sql;

import java.sql.PreparedStatement;
import java.sql.SQLException;

public class SQLUpdateExecutor extends SQLExecutor<Integer> {

    SQLUpdateExecutor() {
    }

    @Override
    public Integer execute(Object... values) throws SQLException {
        var statement = getStatement();
        if (statement instanceof PreparedStatement preparedStatement) {
            this.setPreparedStatementValues(values);
            return preparedStatement.executeUpdate();
        }
        return statement.executeUpdate(getSql());
    }
}
