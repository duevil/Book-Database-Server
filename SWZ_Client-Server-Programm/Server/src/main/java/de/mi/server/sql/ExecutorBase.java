package de.mi.server.sql;

import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Statement;

/**
 * Abstrakte Basis für einen {@link Executor}, welcher ein {@link Statement} und einen SQL-Befehl als Feld speichert
 *
 * @param <T> Der Typ des Ergebnisses der {@link Executor#execute(Object...) Ausführung}
 * @author Malte Kasolowsky <code>m30114</code>
 */
public abstract class ExecutorBase<T> implements Executor<T> {
    private final Statement statement;
    private final String sql;

    /**
     * Vererbbarer Konstruktor; speichert ein {@link PreparedStatement}, wodurch ein SQl-Befehl nicht nötig ist
     *
     * @param statement Das Statement, welches ausgeführt werden soll
     */
    protected ExecutorBase(PreparedStatement statement) {
        this(statement, null);
    }

    /**
     * Vererbbarer Konstruktor; speichert ein {@link Statement} und einen entsprechenden SQL-Befehl
     *
     * @param statement Das Statement, welches ausgeführt werden soll
     * @param sql       Der SQL-Befehl, welcher mittels des Statements ausgeführt werden soll
     */
    protected ExecutorBase(Statement statement, String sql) {
        this.statement = statement;
        this.sql = sql;
    }

    /**
     * Getter für das gespeicherte {@link Statement}
     *
     * @return Das gespeicherte Statement
     */
    protected Statement getStatement() {
        return statement;
    }

    /**
     * Getter für den gespeicherten SQL-Befehl
     *
     * @return Den gespeicherten SQl-Befehl
     */
    protected String getSql() {
        return sql;
    }

    /**
     * Sofern das gespeicherte {@link Statement} ein {@link PreparedStatement} ist,
     * versucht die Parameter des Statements mit den übergebenen Argumenten zu füllen
     *
     * @param values Die Argumente des PreparedStatements
     * @throws SQLException           Wenn beim Füllen der Parameter diese Ausnahme geworfen wird
     * @throws IllegalCallerException Wenn das gespeicherte Statement kein PreparedStatement ist
     */
    protected void setPreparedStatementValues(Object... values) throws SQLException, IllegalCallerException {
        if (statement instanceof PreparedStatement preparedStatement) {
            for (int i = 0; i < values.length; i++) {
                preparedStatement.setObject(i + 1, values[i]);
            }
        } else throw new IllegalCallerException("executor does not use a prepared statement");
    }
}
