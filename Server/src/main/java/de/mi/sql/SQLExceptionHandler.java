package de.mi.sql;

import java.sql.SQLException;
import java.util.logging.Level;
import java.util.logging.Logger;

public final class SQLExceptionHandler {
    private SQLExceptionHandler() {
    }

    @Deprecated(since = "0.4.2")
    public static void handle(SQLException e) {
        handle(e, Logger.getAnonymousLogger());
    }

    @Deprecated(since = "0.4.2")
    public static void handle(SQLException e, String sql) {
        handle(e, sql, Logger.getAnonymousLogger());
    }

    public static void handle(SQLException e, Logger logger) throws IllegalArgumentException {
        if (e == null) throw new IllegalArgumentException("exception to be handled must not be null");
        logger.log(Level.WARNING, e, () -> String.format(
                "A SQL exception was thrown [SQL state = %s, error code = %d]",
                e.getSQLState(),
                e.getErrorCode()
        ));
    }

    public static void handle(SQLException e, String sql, Logger logger) {
        handle(e, logger);
        logger.log(Level.INFO, "Causing SQL statement: {0}", sql);
    }
}
