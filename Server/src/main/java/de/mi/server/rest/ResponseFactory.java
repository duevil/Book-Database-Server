package de.mi.server.rest;

import de.mi.server.sql.SQLExceptionHandler;
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
            Throwable entity = new Throwable("An error during database communication occurred");
            response = Response.serverError().entity(entity);
        } catch (IllegalArgumentException e) {
            LOGGER.log(Level.INFO, "Client request had bad arguments", e);
            response = Response.status(Response.Status.BAD_REQUEST).entity(e);
        } catch (RuntimeException e) {
            LOGGER.log(Level.SEVERE, "Unexpected exception was thrown", e);
            response = Response.serverError().entity(e);
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
        Throwable entity = new IllegalCallerException("Insufficient privileges to perform the request");
        return Response.status(Response.Status.UNAUTHORIZED).entity(entity).build();
    }
}
