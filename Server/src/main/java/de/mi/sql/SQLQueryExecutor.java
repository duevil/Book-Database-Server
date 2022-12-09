package de.mi.sql;

import de.mi.mapper.Mapper;

import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

public class SQLQueryExecutor extends SQLExecutor {
    private final List<Map<String, Object>> queryResult = new LinkedList<>();

    SQLQueryExecutor() {
        super();
    }

    @Override
    public void execute() throws SQLException {
        var statement = getStatement();
        var resultSet = statement instanceof PreparedStatement preparedStatement
                ? preparedStatement.executeQuery()
                : statement.executeQuery(getSql());
        var resultSetMetaData = resultSet.getMetaData();
        var typeMap = statement.getConnection().getTypeMap();
        var columnCount = resultSetMetaData.getColumnCount();
        queryResult.clear();
        while (resultSet.next()) {
            var map = new HashMap<String, Object>(columnCount);
            for (int i = 1; i <= columnCount; i++) {
                map.put(
                        resultSetMetaData.getColumnName(i),
                        resultSet.getObject(i, typeMap)
                );
            }
            queryResult.add(Collections.unmodifiableMap(map));
        }
    }

    public <T> List<T> getMappedList(Mapper<T> mapper) throws SQLException {
        execute();
        return queryResult.stream().map(mapper).toList();
    }
}
