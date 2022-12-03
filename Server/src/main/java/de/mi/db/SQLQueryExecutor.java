package de.mi.db;

import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;

public class SQLQueryExecutor {
    private final Statement statement;
    private final String sql;

    public SQLQueryExecutor(PreparedStatement statement) {
        this(Objects.requireNonNull(statement, "prepared statement"), null);
    }

    public SQLQueryExecutor(String sql) {
        this(null, Objects.requireNonNull(sql, "sql string"));
    }

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

    public List<Map<String, Object>> getMapList() {
        try {
            if (statement instanceof PreparedStatement) {
                return Collections.unmodifiableList(getMapList(statement, null));
            } else {
                try (
                        var con = DBConnection.get().connection();
                        var stmt = con.createStatement()
                ) {
                    return Collections.unmodifiableList(getMapList(stmt, sql));
                }
            }
        } catch (SQLException e) {
            SQLExceptionHandler.handle(e);
            return Collections.emptyList();
        }
    }

    public <T> List<T> getMappedList(Function<Map<String, Object>, T> mapper) {
        return getMapList().stream().map(mapper).toList();
    }
}
