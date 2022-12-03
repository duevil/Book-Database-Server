package de.mi.db;

import java.io.PrintStream;
import java.sql.SQLException;

public final class SQLExceptionHandler {
    private SQLExceptionHandler() {
    }

    public static void handle(SQLException e) {
        handle(e, System.err);
    }

    public static void handle(SQLException e, PrintStream out) {
        if (e == null) throw new IllegalArgumentException("exception to be handled must not be null");
        out.println("--- A SQLException occurred ---");
        for (
                SQLException ex = e;
                ex != null;
                ex = e.getNextException()
        ) {
            out.printf(
                    """
                            Message: %s
                            SQLState: %s
                            ErrorCode: %d
                            """.stripIndent(),
                    e.getMessage(),
                    e.getSQLState(),
                    e.getErrorCode()
            );
        }
    }
}
