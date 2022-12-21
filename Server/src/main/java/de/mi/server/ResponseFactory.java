package de.mi.server;

import de.mi.sql.SQLExceptionHandler;
import jakarta.ws.rs.core.Response;

import java.sql.SQLException;
import java.util.logging.Level;
import java.util.logging.Logger;

final class ResponseFactory {
    private static final Logger LOGGER = Logger.getLogger("org.glassfish");

    private ResponseFactory() {
    }

    public static <R, T> Response create(ExFunction<R, T> function, T t) {
        var response = Response.noContent();
        try {
            R entity = function.apply(t);
            if (!(function instanceof ExConsumer)) response = Response.ok(entity);
        } catch (SQLException e) {
            SQLExceptionHandler.handle(e, LOGGER);
            response = Response.serverError();
        } catch (IllegalArgumentException e) {
            LOGGER.log(Level.INFO, "Client request had bad arguments", e);
            response = Response.status(Response.Status.BAD_REQUEST);
        }
        return response.build();
    }

    public static <T> Response create(ExSupplier<T> supplier) {
        return create(supplier, null);
    }

    public static <T> Response create(ExConsumer<T> consumer, T t) {
        return ResponseFactory.<Void, T>create(consumer, t);
    }

    public static Response createUnauthorized() {
        return Response.status(Response.Status.UNAUTHORIZED).build();
    }
}
