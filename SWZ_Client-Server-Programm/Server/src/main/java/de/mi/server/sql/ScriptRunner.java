package de.mi.server.sql;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.Charset;
import java.nio.file.Path;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Scanner;

/**
 * Implementiert einen {@link Executor} zum Ausführen eines SQl-Scripts, also mehrerer SQL-Befehle auf einmal
 *
 * @author Malte Kasolowsky <code>m30114</code>
 */
class ScriptRunner extends ExecutorBase<Void> {
    private static final Logger LOGGER = LogManager.getLogger(ScriptRunner.class);
    private final InputStream data;

    /**
     * Konstruktor; speichert ein {@link Statement} und einen {@link InputStream}
     *
     * @param statement Das Statement, über das die SQL-Befehle ausgeführt werden sollen
     * @param data      Ein Datenstrom, welcher die SQL-Befehle beinhaltet
     */
    public ScriptRunner(Statement statement, InputStream data) {
        super(statement, null);
        this.data = data;
        LOGGER.trace("Created ScriptRunner with Statement [{}] and InputStream [{}]", statement, data);
    }

    /**
     * Liest eine Datei am angegebenen {@link Path} ein. Dabei wird der Inhalt Zeile für Zeile durchgegangen,
     * bis ein Semicolon am Zeilenende erkannt wird,
     * wobei dann der bis dahin gelesene Inhalt als SQL-Befehl über die übergebene {@link Connection} ausgeführt wird.
     * Dabei werden Zeilenkommentare ignoriert. Ist bei der Connection auto-commit aktiv,
     * so wird dieses während des Ausführens der Script-Datei temporär deaktiviert.
     * Für das Einlesen wird das {@link Charset#defaultCharset()} des Systems genutzt
     *
     * @param connection Die Connection zu der Datenbank, für die das Script ausgeführt werden soll
     * @param path       Der Path zur auszuführenden Script-Datei
     * @throws IOException Wenn beim Öffnen der Datei in I/O-Fehler auftritt
     * @deprecated Klasse wurde als Implementierung von {@link ExecutorBase} ge-refactored;
     * die Funktionalität dieser Methode wurde in {@link ScriptRunner#execute(Object...)} übernommen
     */
    @Deprecated(since = "0.2.7")
    public static void runFile(Connection connection, Path path) throws IOException {
        try (
                var statement = connection.createStatement();
                var scanner = new Scanner(path, Charset.defaultCharset())
        ) {
            // save original auto commit value to restore it later
            boolean origAutoCommit = connection.getAutoCommit();
            executionLoop(connection, statement, scanner);
            // restore previous auto commit state
            connection.setAutoCommit(origAutoCommit);
        } catch (SQLException e) {
            //SQLExceptionHandler.handle(e);
        }
    }

    /**
     * List die Daten des gespeicherten {@link InputStream InputStreams} in einen {@link Scanner ein},
     * mit welchem dann die SQL-Befehle
     * {@link ScriptRunner#executionLoop(Connection, Statement, Scanner) ausgeführt} werden,
     * wobei Auto-Commit vor der Ausführung auf false
     * und nach erfolgreicher Ausführung wieder in den Ursprungszustand gesetzt wird
     *
     * @param values Wird für diese Implementierung nicht genutzt
     * @return null
     * @throws SQLException       Wenn bei der Ausführung eine solche Ausnahme geworfen wird
     * @throws IllegalAccessError Wenn das gespeicherte Statement ein {@link PreparedStatement} ist
     */
    @Override
    public Void execute(Object... values) throws SQLException {
        LOGGER.trace("Executing script");
        try (var scanner = new Scanner(data, Charset.defaultCharset())) {
            Statement statement = getStatement();
            if (statement instanceof PreparedStatement) {
                LOGGER.error("Execution can not be completed for prepared statement");
                throw new IllegalAccessError("Execution can not be completed for prepared statement");
            }
            Connection connection = statement.getConnection();
            // save original auto commit value to restore it later
            boolean origAutoCommit = connection.getAutoCommit();
            executionLoop(connection, statement, scanner);
            // restore previous auto commit state
            LOGGER.trace("Restoring auto commit to {}", origAutoCommit);
            connection.setAutoCommit(origAutoCommit);
        }
        LOGGER.trace("Script execution completed");
        return null;
    }

    /**
     * Liest alle SQl-Befehle aus dem gegebenen {@link Scanner} ein
     * und führt diese aus mit dem gegebenen {@link Statement} aus
     * <p>
     * Dabei wird vor dem Ausführen Auto-Commit auf false gesetzt, tritt während der Ausführung ein Fehler auf,
     * so wird ein Rollback ausgeführt,
     * alle Änderungen werden also nur als eine einzelne, gesamte Transaktion durchgeführt
     *
     * @param connection Die Verbindung, über die das Statement ausgeführt wird
     * @param statement  Das Statement, über das die ausgelesenen SQL-Befehle ausgeführt werden
     * @param scanner    Der Scanner, aus dem die SQL-Befehle eingelesen werden
     * @throws SQLException Wenn bei der Ausführung eine solche Ausnahme geworfen wird
     */
    private static void executionLoop(Connection connection, Statement statement, Scanner scanner) throws SQLException {
        var sb = new StringBuilder();
        // set auto commit to false, ensuring the sql script will only be executed as one
        LOGGER.trace("Setting auto commit to false");
        connection.setAutoCommit(false);
        try {
            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();
                LOGGER.trace("Reading next script line: {}", line);
                // ignore blank line or line comments
                if (!line.startsWith("--") &&
                    !line.startsWith("#") &&
                    !line.isBlank()) {
                    LOGGER.trace("Appending line to sql command: {}", line);
                    sb.append(line).append(' ');
                } else {
                    LOGGER.trace("Ignoring line: {}", line);
                }
                // execute a sql command when finding a terminating semicolon
                if (!sb.isEmpty() && line.endsWith(";")) {
                    LOGGER.debug("Executing sql command: {}", sb);
                    statement.execute(sb.toString());
                    sb = new StringBuilder();
                }
            }
        } catch (SQLException e) {
            LOGGER.error("Error while executing sql script: {}", e.getMessage());
            LOGGER.trace("Rolling back changes");
            connection.rollback();
            throw e;
        }
        LOGGER.trace("Committing changes");
        connection.commit();
    }
}
