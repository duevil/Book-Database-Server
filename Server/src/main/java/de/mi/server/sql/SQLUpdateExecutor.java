package de.mi.server.sql;

import java.sql.PreparedStatement;
import java.sql.SQLException;

public class SQLUpdateExecutor extends SQLExecutor<Integer> {

    public SQLUpdateExecutor(PreparedStatement statement) {
        super(statement);
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
