package de.mi.server;

import com.mysql.cj.exceptions.MysqlErrorNumbers;
import com.mysql.cj.jdbc.exceptions.CommunicationsException;
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

/**
 * Singleton-Klasse zum Öffnen und Verwalten der Datenbankverbindung
 *
 * @author Malte Kasolowsky <code>m30114</code>
 */
public final class DBConnection {
    private static final Logger LOGGER = Logger.getLogger("org.glassfish");

    private final Connection connection;

    /**
     * Konstruktor; öffnet eine neue {@link Connection Datenbankverbindung};
     * versucht sich mittels der in den Properties Userdaten in die Datenbank einzuwählen.
     * Falls dies scheitern sollte, wird die Eingabe des Datenbank-root-user-Passworts gefordert
     * und die Datenbank initialisiert
     *
     * @throws SQLException          Falls beim Öffnen der Verbindung eine solche Ausnahme geworfen wurde
     * @throws IllegalStateException Wenn keine Verbindung zum Datenbankserver aufgebaut werden konnte
     *                               oder die Eingabe des root-Passworts abgebrochen wurde
     */
    private DBConnection() throws SQLException, IllegalStateException {
        Connection con;
        ResourceBundle resources = ResourceBundle.getBundle("database");
        String baseUrl = resources.getString("baseUrl");
        String port = resources.getString("port");
        String databaseName = resources.getString("name");
        String user = resources.getString("user");
        String password = resources.getString("password");
        String url = String.format("%s:%s/%s", baseUrl, port, databaseName);
        try {
            con = DriverManager.getConnection(url, user, password);
        } catch (CommunicationsException e) {
            throw new IllegalStateException("Connection to Database Server could not be established", e);
        } catch (SQLException e) {
            if (e.getErrorCode() != MysqlErrorNumbers.ER_BAD_DB_ERROR) throw e;

            LOGGER.warning(() -> """
                    Unable to connect to database at [%s]
                    Database was not found because it properly does not exist.
                    Trying to connect to root server and to create database and user...""".formatted(url));
            String pass = ConsolePrompter.prompt("""
                    The 'Informatik'-database could not be found and needs to be created.
                    Please enter the database server's root password to allow creation.
                    The password will not be saved and no further actions will be performed.""");
            LOGGER.info(() -> String.format(
                    "Connecting to root and creating database '%s' and user '%s'",
                    databaseName,
                    user
            ));
            try (Connection c = DriverManager.getConnection(baseUrl, "root", pass);
                 var is = ClassLoader.getSystemResourceAsStream("informatik-schema.sql")) {
                ExecutorFactory.createScriptRunner(c.createStatement(), is).execute();
                LOGGER.info("Database and user created. Retrying initial connection attempt");
            } catch (IOException ex) {
                throw new UncheckedIOException(ex);
            }
            con = DriverManager.getConnection(url, user, password);
        }
        LOGGER.info(() -> "Connected to database at [%s]".formatted(url));
        connection = con;
    }

    /**
     * Führt eine sql-Ressourcendatei mittels eines
     * {@link ExecutorFactory#createScriptRunner(Statement, java.io.InputStream) ScriptRunners} aus
     *
     * @param name Der Name des auszuführenden Scripts
     * @throws UncheckedIOException Falls beim Öffnen der Ressource eine {@link IOException} geworfen wird
     */
    public static void executeResourceScript(String name) throws UncheckedIOException {
        try (var is = ClassLoader.getSystemResourceAsStream(name)) {
            ExecutorFactory.createScriptRunner(createStatement(), is).execute();
        } catch (SQLException e) {
            SQLExceptionHandler.handle(e, LOGGER);
            throw new IllegalArgumentException("Unable to execute script file", e);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    /**
     * Erstellt ein neues  {@link Statement}
     *
     * @return Ein neues Statement
     */
    public static Statement createStatement() {
        try {
            return Singleton.INSTANCE.dbCon.connection.createStatement();
        } catch (SQLException e) {
            SQLExceptionHandler.handle(e, LOGGER);
            throw new NoSuchElementException("No statement was created", e);
        }
    }

    /**
     * Erstellt ein neues {@link PreparedStatement} mit dem übergebenen SQL-Befehl
     *
     * @param sql Der SQL-Befehl zum Initialisieren des Statements
     * @return Ein neues PreparesStatement
     */
    public static PreparedStatement prepareStatement(String sql) {
        try {
            return Singleton.INSTANCE.dbCon.connection.prepareStatement(sql);
        } catch (SQLException e) {
            SQLExceptionHandler.handle(e, sql, LOGGER);
            throw new NoSuchElementException("No prepared statement was created", e);
        }
    }

    /**
     * Schließt die Datenbankverbindung
     */
    public static void close() {
        try {
            Singleton.INSTANCE.dbCon.connection.close();
            LOGGER.info("Database connection closed");
        } catch (SQLException e) {
            SQLExceptionHandler.handle(e, LOGGER);
        }
    }

    /**
     * Ein enum, welches die Singleton-Instanz der Klasse speichert;
     * durch die Nutzung eines enums statt eines statischen Feldes sollte Multithreading-Sicherheit garantiert werden
     */
    private enum Singleton {
        INSTANCE;
        private final DBConnection dbCon;

        /**
         * Erstellt eine {@link DBConnection#DBConnection() neue Klasseninstanz}
         *
         * @throws ExceptionInInitializerError Falls beim Erzeugen einer Instanz eine {@link SQLException} geworfen wird
         */
        Singleton() throws ExceptionInInitializerError {
            try {
                dbCon = new DBConnection();
            } catch (SQLException e) {
                throw new ExceptionInInitializerError(e);
            }
        }
    }
}
