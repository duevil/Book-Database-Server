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

public final class DBConnection {
    private static final String BASE_URL = "jdbc:mysql://localhost";
    private static final String DATABASE_NAME = "informatik";
    private static final String SQL_SCRIPTS_PATH = "sql-scripts";
    private static final Path SCHEMA_SQL = Path.of(SQL_SCRIPTS_PATH, "informatik-schema.sql");
    private static final DBConnection INSTANCE;

    static {
        DBConnection instance = null;
        try {
            instance = new DBConnection("minf", "prog3");
        } catch (SQLException e) {
            SQLExceptionHandler.handle(e);
        }
        INSTANCE = instance;
    }

    private final Connection connection;

    private DBConnection(String user, String password) throws SQLException {
        var url = BASE_URL + '/' + DATABASE_NAME;
        Connection con;
        try {
            con = DriverManager.getConnection(url, user, password);
        } catch (SQLException e) {
            SQLExceptionHandler.handle(e);
            System.err.println("""
                    Database properly does not exist.
                    Trying to connect to root server and to create database and user...""");
            String pass = JOptionPane.showInputDialog("Enter SQL server root user password:");
            if (pass == null) throw new ExceptionInInitializerError("password input was canceled");
            try (Connection c = DriverManager.getConnection(BASE_URL, "root", pass)) {
                SQLExecutorFactory.createScriptRunner(SCHEMA_SQL)
                        .setStatement(c.createStatement())
                        .get()
                        .execute();
            }
            con = DriverManager.getConnection(url, user, password);
        }
        connection = con;
    }

    public static DBConnection get() throws IllegalStateException {
        if (INSTANCE == null)
            throw new IllegalStateException("no connection to a database was established");
        return INSTANCE;
    }

    public static PreparedStatement prepareStatement(String sql) throws SQLException {
        return get().connection.prepareStatement(sql);
    }

    public static Statement createStatement() throws SQLException {
        return get().connection.createStatement();
    }
}
