package de.mi.server.sql;

import java.sql.PreparedStatement;
import java.sql.SQLException;

/**
 * Implementiert einen {@link Executor} zum Ausführen von Updates für die Datenbank
 *
 * @author Malte Kasolowsky <code>m30114</code>
 */
class UpdateExecutor extends ExecutorBase<Integer> {

    /**
     * Konstruktor; speichert ein {@link PreparedStatement}
     *
     * @param statement Das Statement, welches ausgeführt werden soll
     */
    public UpdateExecutor(PreparedStatement statement) {
        super(statement);
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
            this.setPreparedStatementValues(values);
            return preparedStatement.executeUpdate();
        }
        return statement.executeUpdate(getSql());
    }
}
