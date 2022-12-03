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
    private static final String SQL_SCRIPT_PATH = "sql";
    private static final Path DATABASE_SQL
            = Path.of(SQL_SCRIPT_PATH, DATABASE_NAME + "-database.sql");
    private static final Path INSERTIONS_SQL
            = Path.of(SQL_SCRIPT_PATH, DATABASE_NAME + "-insertions.sql");
    private static DBConnection instance;
    private final Connection connection;

    private DBConnection(String user, String password) throws SQLException, IOException {
        try (var con = DriverManager.getConnection(BASE_URL, "root", null)) {
            SQLScriptRunner.runFile(con, DATABASE_SQL);
            SQLScriptRunner.runFile(con, INSERTIONS_SQL);
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
