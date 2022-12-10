package de.mi.server;

import de.mi.db.DBConnection;
import de.mi.sql.SQLExceptionHandler;
import de.mi.sql.SQLExecutorFactory;

import java.nio.file.Path;
import java.sql.SQLException;

public class Main {
    private static final String SQL_SCRIPTS_PATH = "sql-scripts";
    private static final Path TABLE_SQL = Path.of(SQL_SCRIPTS_PATH, "literature-tables.sql");
    private static final Path DATA_SQL = Path.of(SQL_SCRIPTS_PATH, "literature-data.sql");
    private static final Path SUBFIELDS_SQL = Path.of(SQL_SCRIPTS_PATH, "subfields-data.sql");
    private static final String HOST = "0.0.0.0";
    private static final int PORT = 8080;
    private static final String SERVER_NAME = "informatik";

    public static void main(String[] args) {
        try {
            SQLExecutorFactory.createScriptRunner(TABLE_SQL)
                    .setStatement(DBConnection.createStatement()).get()
                    .execute();
            SQLExecutorFactory.createScriptRunner(SUBFIELDS_SQL)
                    .setStatement(DBConnection.createStatement()).get()
                    .execute();
            SQLExecutorFactory.createScriptRunner(DATA_SQL)
                    .setStatement(DBConnection.createStatement()).get()
                    .execute();
        } catch (SQLException e) {
            SQLExceptionHandler.handle(e, System.out);
        }

        Server.start(HOST, PORT, SERVER_NAME, LiteratureRest.getApplicationClass());
    }
}
