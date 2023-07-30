package de.mi.server.sql;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

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

/**
 * Implementiert einen {@link Executor} zum Ausführen einer Daten-Query
 *
 * @param <T> Der Objekttyp, der aus den geladenen Daten erzeugt werden soll
 * @author Malte Kasolowsky <code>m30114</code>
 */
class QueryExecutor<T> extends ExecutorBase<List<T>> {
    private static final Logger LOGGER = LogManager.getLogger(QueryExecutor.class);
    private final Mapper<T> mapper;

    /**
     * Konstruktor; speichert ein {@link PreparedStatement} und einen {@link Mapper}
     *
     * @param statement Das auszuführende Statement
     * @param mapper    Der Mapper, mit welchem die geladenen Daten in Objekte umgewandelt werden sollen
     */
    public QueryExecutor(PreparedStatement statement, Mapper<T> mapper) {
        super(statement);
        this.mapper = mapper;
        LOGGER.trace("Created QueryExecutor with PreparedStatement [{}] and Mapper [{}]", statement, mapper);
    }

    /**
     * Konstruktor; speichert ein {@link Statement}, einen SQL-Befehl und einen {@link Mapper}
     *
     * @param statement Das auszuführende Statement
     * @param sql       Der auszuführende SQL-Befehl
     * @param mapper    Der Mapper, mit welchem die geladenen Daten in Objekte umgewandelt werden sollen
     */
    public QueryExecutor(Statement statement, String sql, Mapper<T> mapper) {
        super(statement, sql);
        this.mapper = mapper;
        LOGGER.trace("Created QueryExecutor with Statement [{}], SQL [{}] and Mapper [{}]", statement, sql, mapper);
    }

    /**
     * Führt das gespeicherte {@link Statement} aus,
     * d.h. entweder das Statement als {@link PreparedStatement} mit den übergebenen Argumenten,
     * oder das Statement mit dem gespeicherten SQl-Befehl
     * <p>
     * Das aus dem Statement resultierende {@link ResultSet} wird dann schrittweise eingelesen
     * und die erhaltenden Daten mit ihren zugehörigen Spaltennamen gespeichert,
     * welche dann mittel des gespeicherten {@link Mapper Mappers} in eine {@link List}
     * mit den entsprechenden Objekten umgewandelt werden
     *
     * @param values Die Parameter für das Statement, sofern dieses ein {@link PreparedStatement} ist
     * @return Eine Liste, welche die ausgelesenen Objekte enthält
     * @throws SQLException Wenn beim Ausführen eine solche Ausnahme geworfen wird
     */
    @Override
    public List<T> execute(Object... values) throws SQLException {
        var queryResult = new LinkedList<Map<String, Object>>();
        Statement statement = getStatement();
        Map<String, Class<?>> typeMap = statement.getConnection().getTypeMap();

        LOGGER.debug("Executing query [{}]", statement);
        ResultSet resultSet;
        if (statement instanceof PreparedStatement preparedStatement) {
            this.setPreparedStatementValues(values);
            resultSet = preparedStatement.executeQuery();
        } else {
            resultSet = statement.executeQuery(getSql());
        }

        ResultSetMetaData resultSetMetaData = resultSet.getMetaData();
        int columnCount = resultSetMetaData.getColumnCount();
        LOGGER.trace("Reading {} columns", columnCount);

        while (resultSet.next()) {
            HashMap<String, Object> map = new HashMap<>(columnCount);
            for (int i = 1; i <= columnCount; i++) {
                LOGGER.trace("Reading column {} ({})", i, resultSetMetaData.getColumnName(i));
                map.put(
                        resultSetMetaData.getColumnName(i),
                        resultSet.getObject(i, typeMap)
                );
            }
            queryResult.add(Collections.unmodifiableMap(map));
        }

        LOGGER.trace("Mapping {} rows", queryResult.size());
        return queryResult.stream().map(mapper).toList();
    }
}
