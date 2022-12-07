package de.mi.db;

import java.io.IOException;
import java.io.PrintStream;
import java.nio.file.Path;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;

public final class DBConnection {
    private static final PrintStream OUT = System.err;
    private static final String BASE_URL = "jdbc:mysql://localhost";
    private static final String DATABASE_NAME = "informatik";
    private static final String SQL_SCRIPTS_PATH = "sql";
    private static final Path SCHEMA_SQL = Path.of(SQL_SCRIPTS_PATH,"informatik-schema.sql");
    private static final Path DATA_SQL = Path.of(SQL_SCRIPTS_PATH,"books-data.sql");
    private static DBConnection instance;
    private final Connection connection;

    private DBConnection(String user, String password) throws SQLException, IOException {
        try (var con = DriverManager.getConnection(BASE_URL, "root", null)) {
            SQLScriptRunner.runFile(con, SCHEMA_SQL);
            SQLScriptRunner.runFile(con, DATA_SQL);
        }
        connection = DriverManager.getConnection(BASE_URL + '/' + DATABASE_NAME, user, password);
    }

    public static DBConnection get() {
        if (instance == null) throw new IllegalStateException("no connection to database has been opened yet");
        return instance;
    }

    public static void open(String user, String password) {
        try {
            instance = new DBConnection(user, password);
        } catch (IOException e) {
            OUT.println("--- An exception was thrown ---");
            Throwable ex = e;
            do {
                OUT.println(ex);
                ex = e.getCause();
                if (ex != null) OUT.println("--- Caused by ---");
            } while (ex != null);
        } catch (SQLException e) {
            SQLExceptionHandler.handle(e, OUT);
        }
    }

    public Connection connection() {
        return connection;
    }
}
