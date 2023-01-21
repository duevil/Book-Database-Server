package de.mi.server;

import de.mi.server.sql.ExecutorFactory;
import de.mi.server.sql.SQLExceptionHandler;

import java.io.IOException;
import java.io.UncheckedIOException;
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
    private final Connection connection;

    private DBConnection() throws SQLException {
        connection = createConnection();
    }

    private static Connection createConnection() throws SQLException {
        ResourceBundle resources = ResourceBundle.getBundle("database");
        String baseUrl = resources.getString("baseUrl");
        String databaseName = resources.getString("databaseName");
        String user = resources.getString("user");
        String password = resources.getString("password");
        String url = baseUrl + '/' + databaseName;
        try {
            return DriverManager.getConnection(url, user, password);
        } catch (SQLException e) {
            SQLExceptionHandler.handle(e, LOGGER);
            LOGGER.info("""
                    Database properly does not exist.
                    Trying to connect to root server and to create database and user...""");
            String pass = SimplePrompter.getDialogInput("""
                    The 'Informatik'-database does not exist and needs to be created.
                    Please enter the database server's root password to allow creation.
                    The password will not be saved and no further actions will be performed.""");
            if (pass == null) throw new IllegalStateException("password input was canceled");

            try (Connection c = DriverManager.getConnection(baseUrl, "root", pass);
                 var is = ClassLoader.getSystemResourceAsStream("informatik-schema.sql")) {
                ExecutorFactory.createScriptRunner(c.createStatement(), is).execute();
            } catch (IOException ex) {
                throw new UncheckedIOException(ex);
            }
        }
        return DriverManager.getConnection(url, user, password);
    }

    public static void executeResourceScript(String name) throws UncheckedIOException {
        try (var is = ClassLoader.getSystemResourceAsStream(name)) {
            ExecutorFactory.createScriptRunner(createStatement(), is).execute();
        } catch (SQLException e) {
            SQLExceptionHandler.handle(e, LOGGER);
            throw new IllegalArgumentException("unable to execute script file", e);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
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

    public static PreparedStatement prepareStatement(String sql) {
        try {
            return Singleton.INSTANCE.dbCon.connection.prepareStatement(sql);
        } catch (SQLException e) {
            SQLExceptionHandler.handle(e, sql, LOGGER);
            throw new NoSuchElementException("no prepared statement was created", e);
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

        Singleton() throws ExceptionInInitializerError {
            try {
                dbCon = new DBConnection();
            } catch (SQLException e) {
                throw new ExceptionInInitializerError(e);
            }
        }
    }
}
