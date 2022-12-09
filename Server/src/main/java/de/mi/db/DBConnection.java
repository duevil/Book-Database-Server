package de.mi.db;

import de.mi.sql.SQLExceptionHandler;
import de.mi.sql.SQLExecutorFactory;

import java.io.IOException;
import java.io.PrintStream;
import java.nio.file.Path;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Statement;

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
            SQLExecutorFactory.createScriptRunner(SCHEMA_SQL)
                    .forStatement(con.createStatement())
                    .get()
                    .execute();
        }
        connection = DriverManager.getConnection(BASE_URL + '/' + DATABASE_NAME, user, password);
        SQLExecutorFactory.createScriptRunner(TABLE_SQL)
                .forStatement(connection.createStatement())
                .get()
                .execute();
        SQLExecutorFactory.createScriptRunner(SUBFIELDS_SQL)
                .forStatement(connection.createStatement())
                .get()
                .execute();
        SQLExecutorFactory.createScriptRunner(DATA_SQL)
                .forStatement(connection.createStatement())
                .get()
                .execute();
    }

    public static DBConnection get() {
        if (INSTANCE == null)
            throw new IllegalStateException("no connection to a database could was established");
        return INSTANCE;
    }

    public static PreparedStatement prepareStatement(String sql) throws SQLException {
        return get().connection.prepareStatement(sql);
    }

    public static Statement createStatement() throws SQLException {
        return get().connection.createStatement();
    }
}
