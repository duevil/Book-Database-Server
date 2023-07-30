package de.mi.server.sql;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.sql.PreparedStatement;
import java.sql.SQLException;

/**
 * Implementiert einen {@link Executor} zum Ausführen von Updates für die Datenbank
 *
 * @author Malte Kasolowsky <code>m30114</code>
 */
class UpdateExecutor extends ExecutorBase<Integer> {
    private static final Logger LOGGER = LogManager.getLogger(UpdateExecutor.class);

    /**
     * Konstruktor; speichert ein {@link PreparedStatement}
     *
     * @param statement Das Statement, welches ausgeführt werden soll
     */
    public UpdateExecutor(PreparedStatement statement) {
        super(statement);
        LOGGER.trace("Created UpdateExecutor with PreparedStatement [{}]", statement);
    }

    /**
     * Führt das Statement mit den übergebenen Argumenten aus
     *
     * @param values Die Parameter für das Statement
     * @return Die Anzahl an Spalten, die durch das Statement verändert wurden
     * @throws SQLException Wenn bei der Ausführung eine solche Ausnahme geworfen wurde
     */
    @Override
    public Integer execute(Object... values) throws SQLException {
        var statement = getStatement();
        if (statement instanceof PreparedStatement preparedStatement) {
            LOGGER.debug("Executing prepared statement with values: {}", values);
            this.setPreparedStatementValues(values);
            return preparedStatement.executeUpdate();
        }
        LOGGER.debug("Executing statement [{}]", getSql());
        return statement.executeUpdate(getSql());
    }
}
