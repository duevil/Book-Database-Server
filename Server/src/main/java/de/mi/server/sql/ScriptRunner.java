package de.mi.server.sql;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.Charset;
import java.nio.file.Path;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Scanner;

@SuppressWarnings({"java:S1133", "java:S1135"}) // TODO: remove suppression
class ScriptRunner extends ExecutorBase<Void> {
    private final InputStream data;

    public ScriptRunner(Statement statement, InputStream data) {
        super(statement, null);
        this.data = data;
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
            var sb = new StringBuilder();
            // save original auto commit value to restore it later
            boolean origAutoCommit = connection.getAutoCommit();
            executionLoop(connection, statement, scanner, sb);
            // restore previous auto commit state
            connection.setAutoCommit(origAutoCommit);
        } catch (SQLException e) {
            SQLExceptionHandler.handle(e);
        }
    }

    private static void executionLoop(
            Connection connection,
            Statement statement,
            Scanner scanner,
            StringBuilder sb) throws SQLException {
        connection.setAutoCommit(false);
        try {
            // set auto commit to false, ensuring the sql script will only be executed as one
            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();
                // ignore blank line or line comments
                if (!line.startsWith("--") &&
                    !line.startsWith("#") &&
                    !line.isBlank()) sb.append(line).append(' ');
                // execute a sql command when finding a terminating semicolon
                if (!sb.isEmpty() && line.endsWith(";")) {
                    statement.execute(sb.toString());
                    sb = new StringBuilder();
                }
            }
        } catch (SQLException e) {
            connection.rollback();
            throw e;
        }
        connection.commit();
    }

    @Override
    public Void execute(Object... values) throws SQLException {
        var sb = new StringBuilder();
        try (var scanner = new Scanner(data, Charset.defaultCharset())) {
            Statement statement = getStatement();
            if (statement instanceof PreparedStatement) {
                throw new IllegalAccessError("execution can not be completed for prepared statement");
            }
            Connection connection = statement.getConnection();
            // save original auto commit value to restore it later
            boolean origAutoCommit = connection.getAutoCommit();
            executionLoop(connection, statement, scanner, sb);
            // restore previous auto commit state
            connection.setAutoCommit(origAutoCommit);
        }
        return null;
    }
}
