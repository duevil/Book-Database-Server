package de.mi.server.sql;

import java.sql.SQLException;

/**
 * Interface zum Ausführen eines SQL-Statements mit entsprechender Rückgabe
 *
 * @param <T> Der Typ des Ergebnisses der Ausführung
 * @author Malte Kasolowsky <code>m30114</code>
 */
public interface Executor<T> {
    /**
     * Führt ein {@link java.sql.Statement} mit beliebigen optionalen Parametern aus
     *
     * @param values Die Parameter für das Statement, sofern dieses ein {@link java.sql.PreparedStatement} ist
     * @return Das Ergebnis der Ausführung
     * @throws SQLException Wenn beim Ausführen des Statements diese Ausnahme geworfen wurde
     */
    T execute(Object... values) throws SQLException;
}
