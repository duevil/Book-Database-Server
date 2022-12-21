package de.mi.sql;

import de.mi.mapper.Mapper;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

public class SQLQueryExecutor<T> extends SQLExecutor<List<T>> {
    private final Mapper<T> mapper;

    SQLQueryExecutor(PreparedStatement statement, Mapper<T> mapper) {
        super(statement);
        this.mapper = mapper;
    }

    SQLQueryExecutor(Statement statement, String sql, Mapper<T> mapper) {
        super(statement, sql);
        this.mapper = mapper;
    }

    @Override
    public List<T> execute(Object... values) throws SQLException {
        var queryResult = new LinkedList<Map<String, Object>>();
        var statement = getStatement();
        var typeMap = statement.getConnection().getTypeMap();

        ResultSet resultSet;
        if (statement instanceof PreparedStatement preparedStatement) {
            this.setPreparedStatementValues(values);
            resultSet = preparedStatement.executeQuery();
        } else resultSet = statement.executeQuery(getSql());

        var resultSetMetaData = resultSet.getMetaData();
        var columnCount = resultSetMetaData.getColumnCount();

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

        return queryResult.stream().map(mapper).toList();
    }
}
