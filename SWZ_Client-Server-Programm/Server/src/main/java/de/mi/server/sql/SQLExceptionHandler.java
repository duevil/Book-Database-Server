package de.mi.server.sql;

import org.apache.logging.log4j.Logger;

import java.sql.SQLException;
import java.util.logging.Level;

/**
 * Utility-Klasse zum Verarbeiten von {@link SQLException SQLExceptions}
 *
 * @author Malte Kasolowsky <code>m30114</code>
 */
public final class SQLExceptionHandler {
    /**
     * Privater Konstruktor; eine Erzeugung einer Klassen-Instanz ist nicht nötig
     */
    private SQLExceptionHandler() {
    }

    /**
     * Liest eine {@link SQLException} aus und schreibt ihre Daten
     * mit entsprechender Meldung in einen {@link java.util.logging.Logger}
     *
     * @param e      Die auszuwertende Exception
     * @param logger Der Logger, mit dem die Ausnahme verarbeitet werden soll
     * @throws IllegalArgumentException Wenn die Ausnahme null ist
     * @deprecated replaced by log4j
     */
    @Deprecated(since = "2.0") // replaced by log4j
    public static void handle(SQLException e, java.util.logging.Logger logger) throws IllegalArgumentException {
        if (e == null) throw new IllegalArgumentException("exception to be handled must not be null");
        for (var thrown : e) {
            logger.log(Level.WARNING, e, (() -> thrown == e ? String.format(
                    "A SQL exception was thrown [SQL state = %s, error code = %d]",
                    e.getSQLState(),
                    e.getErrorCode()
            ) : "Causing exception"));
        }
    }

    /**
     * Liest eine {@link SQLException} aus und schreibt ihre Daten
     * zusammen mit dem auslösenden SQL-Befehle
     * mit entsprechender Meldung in einen {@link java.util.logging.Logger}
     *
     * @param e      Die auszuwertende Exception
     * @param sql    Der ursprüngliche SQL-Befehl, der Grund für den Fehler ist
     * @param logger Der Logger, mit dem die Ausnahme verarbeitet werden soll
     * @throws IllegalArgumentException Wenn die Ausnahme null ist
     * @deprecated replaced by log4j
     */
    @Deprecated(since = "2.0") // replaced by log4j
    public static void handle(SQLException e, String sql, java.util.logging.Logger logger) {
        handle(e, logger);
        logger.log(Level.INFO, "Causing SQL statement: {0}", sql);
    }

    public static void handle(SQLException e, Logger logger) {
        if (e == null) throw new IllegalArgumentException("exception to be handled must not be null");
        for (var thrown : e) {
            if (thrown == e) logger.warn(
                    "A SQL exception was thrown [SQL state = {}, error code = {}]",
                    e.getSQLState(),
                    e.getErrorCode()
            );
            else logger.warn("Causing exception", thrown);
        }
    }

    public static void handle(SQLException e, String sql, Logger logger) {
        handle(e, logger);
        logger.warn("Causing SQL statement: {}", sql);
    }
}
