package de.mi.server.sql;

import java.sql.SQLException;
import java.util.logging.Level;
import java.util.logging.Logger;

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
     * mit entsprechender Meldung in einen anonymen {@link Logger}
     *
     * @param e Die auszuwertende Exception
     * @throws IllegalArgumentException Wenn die Ausnahme null ist
     * @deprecated In einer alten Version nutzte der Handler einen {@link java.io.PrintStream} für die Ausgabe.
     * Da jedoch der Fehler- und Informationsverwaltung des Systems über einen {@link Logger} laufen,
     * ist diese Methode veraltet, da sie ursprünglich {@link System#err} als Ausgabe nutzte
     * und jetzt einen {@link Logger#getAnonymousLogger() anonymen} {@link Logger} nutzt,
     * was nicht erlaubt, einen spezifischen Logger zu ausgabe zu nutzten
     */
    @Deprecated(since = "0.4.2")
    public static void handle(SQLException e) {
        handle(e, Logger.getAnonymousLogger());
    }

    /**
     * Liest eine {@link SQLException} aus und schreibt ihre Daten
     * zusammen mit dem auslösenden SQL-Befehle mit entsprechender Meldung in einen anonymen {@link Logger}
     *
     * @param e   Die auszuwertende Exception
     * @param sql Der ursprüngliche SQL-Befehl, der Grund für den Fehler ist
     * @throws IllegalArgumentException Wenn die Ausnahme null ist
     * @deprecated In einer alten Version nutzte der Handler einen {@link java.io.PrintStream} für die Ausgabe.
     * Da jedoch der Fehler- und Informationsverwaltung des Systems über einen {@link Logger} laufen,
     * ist diese Methode veraltet, da sie ursprünglich {@link System#err} als Ausgabe nutzte
     * und jetzt einen {@link Logger#getAnonymousLogger() anonymen} {@link Logger} nutzt,
     * was nicht erlaubt, einen spezifischen Logger zu ausgabe zu nutzten
     */
    @Deprecated(since = "0.4.2")
    public static void handle(SQLException e, String sql) {
        handle(e, sql, Logger.getAnonymousLogger());
    }

    /**
     * Liest eine {@link SQLException} aus und schreibt ihre Daten
     * mit entsprechender Meldung in einen {@link Logger}
     *
     * @param e      Die auszuwertende Exception
     * @param logger Der Logger, mit dem die Ausnahme verarbeitet werden soll
     * @throws IllegalArgumentException Wenn die Ausnahme null ist
     */
    public static void handle(SQLException e, Logger logger) throws IllegalArgumentException {
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
     * mit entsprechender Meldung in einen {@link Logger}
     *
     * @param e      Die auszuwertende Exception
     * @param sql    Der ursprüngliche SQL-Befehl, der Grund für den Fehler ist
     * @param logger Der Logger, mit dem die Ausnahme verarbeitet werden soll
     * @throws IllegalArgumentException Wenn die Ausnahme null ist
     */
    public static void handle(SQLException e, String sql, Logger logger) {
        handle(e, logger);
        logger.log(Level.INFO, "Causing SQL statement: {0}", sql);
    }
}
