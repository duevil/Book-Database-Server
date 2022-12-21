package de.mi.db;

import de.mi.sql.SQLExceptionHandler;
import de.mi.sql.SQLExecutorFactory;

import javax.swing.JOptionPane;
import java.nio.file.Path;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.NoSuchElementException;
import java.util.ResourceBundle;
import java.util.logging.Logger;

public final class DBConnection {
    private static final Logger LOGGER = Logger.getLogger("org.glassfish");
    private static final String BASE_URL = "jdbc:mysql://localhost";
    private static final String DATABASE_NAME = "informatik";
    private static final String SQL_SCRIPTS_PATH = "sql-scripts";
    private static final Path SCHEMA_SQL = Path.of(SQL_SCRIPTS_PATH, "informatik-schema.sql");
    private static final Path TABLE_SQL = Path.of(SQL_SCRIPTS_PATH, "literature-tables.sql");
    private static final Path DATA_SQL = Path.of(SQL_SCRIPTS_PATH, "literature-data.sql");
    private static final Path SUBFIELDS_SQL = Path.of(SQL_SCRIPTS_PATH, "subfields-data.sql");
    private final Connection connection;

    private DBConnection() throws SQLException {
        connection = createConnection();
    }

    private static Connection createConnection() throws SQLException {
        ResourceBundle resources = ResourceBundle.getBundle("user");
        String username = resources.getString("username");
        String password = resources.getString("password");
        String url = BASE_URL + '/' + DATABASE_NAME;
        try {
            return DriverManager.getConnection(url, username, password);
        } catch (SQLException e) {
            SQLExceptionHandler.handle(e, LOGGER);
            LOGGER.info("""
                    Database properly does not exist.
                    Trying to connect to root server and to create database and user...""");
            String pass = JOptionPane.showInputDialog("Enter SQL server root user password:");
            if (pass == null) throw new Exception("password input was canceled");
            try (Connection c = DriverManager.getConnection(BASE_URL, "root", pass)) {
                SQLExecutorFactory.createScriptRunner(SCHEMA_SQL)
                        .setStatement(c.createStatement())
                        .get()
                        .execute();
            }
            return DriverManager.getConnection(url, username, password);
        }
    }

    public static PreparedStatement prepareStatement(String sql) {
        try {
            return Singleton.INSTANCE.dbCon.connection.prepareStatement(sql);
        } catch (SQLException e) {
            SQLExceptionHandler.handle(e, sql, LOGGER);
            throw new NoSuchElementException("no prepared statement was created", e);
        }
    }

    public static Statement createStatement() {
        try {
            return Singleton.INSTANCE.dbCon.connection.createStatement();
        } catch (SQLException e) {
            SQLExceptionHandler.handle(e, LOGGER);
            throw new NoSuchElementException("no statement was created", e);
        }
    }

    public static void initDataBase() {
        try {
            SQLExecutorFactory.createScriptRunner(TABLE_SQL)
                    .setStatement(createStatement()).get()
                    .execute();
            SQLExecutorFactory.createScriptRunner(SUBFIELDS_SQL)
                    .setStatement(createStatement()).get()
                    .execute();
            SQLExecutorFactory.createScriptRunner(DATA_SQL)
                    .setStatement(createStatement()).get()
                    .execute();
        } catch (SQLException e) {
            SQLExceptionHandler.handle(e, LOGGER);
        }
    }

    public static void close() {
        try {
            Singleton.INSTANCE.dbCon.connection.close();
        } catch (SQLException e) {
            SQLExceptionHandler.handle(e, LOGGER);
        }
    }

    private enum Singleton {
        INSTANCE;
        private final DBConnection dbCon;

        Singleton() {
            try {
                dbCon = new DBConnection();
            } catch (SQLException e) {
                SQLExceptionHandler.handle(e, LOGGER);
                throw new Exception("Exception thrown during initiation of Singleton instance", e);
            }
        }
    }

    private static class Exception extends RuntimeException {
        public Exception(String message, Throwable cause) {
            super(message, cause);
        }

        public Exception(String message) {
            super(message);
        }
    }
}
