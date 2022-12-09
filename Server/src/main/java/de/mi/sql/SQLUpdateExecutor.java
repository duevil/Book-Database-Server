package de.mi.sql;

import java.sql.PreparedStatement;
import java.sql.SQLException;

public class SQLUpdateExecutor extends SQLExecutor {

    SQLUpdateExecutor() {
    }

    @Override
    public void execute() throws SQLException {
        var statement = getStatement();
        if (statement instanceof PreparedStatement preparedStatement) {
            preparedStatement.executeUpdate();
        } else {
            statement.executeUpdate(getSql());
        }
    }
}
