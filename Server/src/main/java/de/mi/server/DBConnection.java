package de.mi.server;

import de.mi.sql.SQLExceptionHandler;
import de.mi.sql.SQLScriptRunner;

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
    private static final String SQL_SCRIPTS_PATH = "sql-scripts";
    private static final Path SCHEMA_SQL = Path.of(SQL_SCRIPTS_PATH, "informatik-schema.sql");
    private static final Path TABLE_SQL = Path.of(SQL_SCRIPTS_PATH, "literature-tables.sql");
    private static final Path DATA_SQL = Path.of(SQL_SCRIPTS_PATH, "literature-data.sql");
    private static final Path SUBFIELDS_SQL = Path.of(SQL_SCRIPTS_PATH, "subfields-data.sql");
    private static final DBConnection INSTANCE;

    static {
        DBConnection instance = null;
        try {
            instance = new DBConnection("minf", "prog3");
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
        INSTANCE = instance;
    }

    private final Connection connection;

    private DBConnection(String user, String password) throws SQLException, IOException {
        try (Connection con = DriverManager.getConnection(BASE_URL, "root", null)) {
            SQLScriptRunner.runFile(con, SCHEMA_SQL);
        }
        connection = DriverManager.getConnection(BASE_URL + '/' + DATABASE_NAME, user, password);
        SQLScriptRunner.runFile(connection, TABLE_SQL);
        SQLScriptRunner.runFile(connection, SUBFIELDS_SQL);
        SQLScriptRunner.runFile(connection, DATA_SQL);
    }

    public static DBConnection get() {
        if (INSTANCE == null)
            throw new IllegalStateException("no connection to a database could have been established");
        return INSTANCE;
    }

    public Connection connection() {
        return connection;
    }
}
