package de.mi.sql;

import de.mi.mapper.Mapper;
import de.mi.server.DBConnection;

import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;

public final class SQLQueryExecutor {
    private final Statement statement;
    private final String sql;

    private SQLQueryExecutor(Statement statement, String sql) {
        this.statement = statement;
        this.sql = sql;
    }

    private static List<Map<String, Object>> getMapList(Statement statement, String sql) throws SQLException {
        var resultSet = statement instanceof PreparedStatement preparedStatement
                ? preparedStatement.executeQuery()
                : statement.executeQuery(sql);
        var resultSetMetaData = resultSet.getMetaData();
        var typeMap = statement.getConnection().getTypeMap();
        var columnCount = resultSetMetaData.getColumnCount();
        var mapList = new LinkedList<Map<String, Object>>();
        while (resultSet.next()) {
            var map = new HashMap<String, Object>(columnCount);
            for (int i = 1; i <= columnCount; i++) {
                map.put(
                        resultSetMetaData.getColumnName(i),
                        resultSet.getObject(i, typeMap)
                );
            }
            mapList.add(Collections.unmodifiableMap(map));
        }
        return mapList;
    }

    public static SQLQueryExecutor forPreparedStatement(PreparedStatement statement) {
        return new SQLQueryExecutor(
                Objects.requireNonNull(statement, "prepared statement"),
                null
        );
    }

    public static SQLQueryExecutor forSQLString(String sql) {
        return new SQLQueryExecutor(
                null,
                Objects.requireNonNull(sql, "sql string")
        );
    }

    private List<Map<String, Object>> getMapList() throws SQLException {
        if (statement instanceof PreparedStatement) {
            return Collections.unmodifiableList(getMapList(statement, null));
        } else try (
                var con = DBConnection.get().connection();
                var stmt = con.createStatement()
        ) {
            return Collections.unmodifiableList(getMapList(stmt, sql));
        }
    }

    public <T> List<T> getMappedList(Mapper<T> mapper) throws SQLException {
        return getMapList().stream().map(mapper).toList();
    }

    public PreparedStatement getPreparedStatement() {
        if (statement instanceof PreparedStatement preparedStatement) return preparedStatement;
        else throw new IllegalCallerException("executor was not created using a prepared statement");
    }
}
