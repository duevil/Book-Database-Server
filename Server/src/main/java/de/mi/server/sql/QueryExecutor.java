package de.mi.server.sql;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

class QueryExecutor<T> extends ExecutorBase<List<T>> {
    private final Mapper<T> mapper;

    public QueryExecutor(PreparedStatement statement, Mapper<T> mapper) {
        super(statement);
        this.mapper = mapper;
    }

    public QueryExecutor(Statement statement, String sql, Mapper<T> mapper) {
        super(statement, sql);
        this.mapper = mapper;
    }

    @Override
    public List<T> execute(Object... values) throws SQLException {
        var queryResult = new LinkedList<Map<String, Object>>();
        Statement statement = getStatement();
        Map<String, Class<?>> typeMap = statement.getConnection().getTypeMap();

        ResultSet resultSet;
        if (statement instanceof PreparedStatement preparedStatement) {
            this.setPreparedStatementValues(values);
            resultSet = preparedStatement.executeQuery();
        } else resultSet = statement.executeQuery(getSql());

        ResultSetMetaData resultSetMetaData = resultSet.getMetaData();
        int columnCount = resultSetMetaData.getColumnCount();

        while (resultSet.next()) {
            HashMap<String, Object> map = new HashMap<>(columnCount);
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
