package de.mi.sql;

import java.io.PrintStream;
import java.sql.SQLException;

public final class SQLExceptionHandler {
    private SQLExceptionHandler() {
    }

    public static void handle(SQLException e) {
        handle(e, System.err);
    }

    public static void handle(SQLException e, String sql) {
        handle(e, sql, System.err);
    }

    public static void handle(SQLException e, PrintStream out) throws IllegalArgumentException {
        if (e == null) throw new IllegalArgumentException("exception to be handled must not be null");
        out.printf(
                """
                        --- A SQLException occurred ---
                        Message: %s
                        SQLState: %s
                        ErrorCode: %d
                        """.stripIndent(),
                e.getMessage(),
                e.getSQLState(),
                e.getErrorCode()
                );
        for (
                SQLException ex = e.getNextException();
                ex != null;
                ex = e.getNextException()
        ) {
            System.out.printf("--- Causing exception ---%n%s", ex);
        }
    }

    public static void handle(SQLException e, String sql, PrintStream out) {
        handle(e, out);
        out.println("SQL Statement: " + sql);
    }
}
