package de.mi.server;

import jakarta.ws.rs.core.Response;

import java.sql.SQLException;
import java.util.logging.Logger;

final class ResponseFactory {
    private static final Logger LOGGER = Logger.getLogger("org.glassfish");

    private ResponseFactory() {

    }

    public static <R, T> Response create(Function<R, T> function, T t) {
        var response = Response.noContent();
        try {
            R entity = function.apply(t);
            if (!(function instanceof Consumer)) response = Response.ok(entity);
        } catch (SQLException e) {
            LOGGER.severe(e::toString);
            response = Response.serverError();
        } catch (IllegalArgumentException e) {
            LOGGER.info(e::toString);
            response = Response.status(Response.Status.BAD_REQUEST);
        }
        return response.build();
    }

    public static <T> Response create(Supplier<T> supplier) {
        return create(supplier, null);
    }

    public static <T> Response create(Consumer<T> consumer, T t) {
        return ResponseFactory.<Void, T>create(consumer, t);
    }

    @FunctionalInterface
    public interface Function<R, T> {
        R apply(T t) throws SQLException, IllegalArgumentException;
    }

    @FunctionalInterface
    public interface Consumer<T> extends Function<Void, T> {
        @Override
        default Void apply(T t) throws SQLException, IllegalArgumentException {
            accept(t);
            return null;
        }

        void accept(T t) throws SQLException, IllegalArgumentException;
    }

    @FunctionalInterface
    public interface Supplier<T> extends Function<T, Void> {
        @Override
        default T apply(Void ignored) throws SQLException, IllegalArgumentException {
            return get();
        }

        T get() throws SQLException, IllegalArgumentException;
    }
}
