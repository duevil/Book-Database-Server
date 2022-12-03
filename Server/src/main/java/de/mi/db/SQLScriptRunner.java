package de.mi.db;

import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Path;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.Scanner;

public final class SQLScriptRunner {

    private SQLScriptRunner() {
    }

    public static void runFile(Connection connection, Path path) throws IOException {
        try (
                var statement = connection.createStatement();
                var scanner = new Scanner(path, Charset.defaultCharset())
        ) {
            // save original auto commit value to restore it later
            var origAutoCommit = connection.getAutoCommit();
            var stringBuilder = new StringBuilder();
            // set auto commit to false, ensuring the sql script will only be executed as one
            connection.setAutoCommit(false);
            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();
                // ignore blank line or line comments
                if (!line.startsWith("--") && !line.isBlank()) stringBuilder.append(line);
                // execute a sql command when finding a terminating semicolon
                if (!stringBuilder.isEmpty() && line.endsWith(";")) {
                    statement.execute(stringBuilder.toString());
                    stringBuilder = new StringBuilder();
                }
            }
            connection.commit();
            // restore previous auto commit state
            connection.setAutoCommit(origAutoCommit);
        } catch (SQLException e) {
            SQLExceptionHandler.handle(e);
        }
    }
}
