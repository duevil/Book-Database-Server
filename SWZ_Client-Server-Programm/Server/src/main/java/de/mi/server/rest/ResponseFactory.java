package de.mi.server.rest;

import de.mi.server.sql.SQLExceptionHandler;
import jakarta.ws.rs.core.Response;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.sql.SQLException;

/**
 * Utility-Klasse zum Erzeugen einer {@link Response}
 * als Resultat des Ausführens einer {@link ExFunction#apply(Object)} Aktion
 *
 * @author Malte Kasolowsky <code>m30114</code>
 */
final class ResponseFactory {
    private static final Logger LOGGER = LogManager.getLogger(ResponseFactory.class);

    /**
     * Privater Konstruktor; eine Erzeugung einer Klassen-Instanz ist nicht nötig
     */
    private ResponseFactory() {
    }

    /**
     * Convenience-Methode zum Erzeugen einer Response
     * als Resultat des Ausführens von {@link ExSupplier#get()}
     *
     * @param supplier Der auszuführende {@link ExSupplier},
     *                 aus dessen Rückgabe die {@link Response} erzeugt werden soll
     * @param <T>      Der Typ des Funktionsergebnisses
     * @return Die erzeugte Response
     */
    public static <T> Response create(ExSupplier<T> supplier) {
        return create(supplier, null);
    }

    /**
     * Erzeugt eine neue {@link Response} aus dem Ergebnis einer {@link ExFunction},
     * wobei eine {@link SQLException}, die beim Ausführen der Aktion geworfen wird,
     * als Response mit dem Status {@link Response.Status#INTERNAL_SERVER_ERROR},
     * eine {@link IllegalArgumentException} mit dem Status {@link Response.Status#BAD_REQUEST}
     * und jede sonstige {@link RuntimeException} ebenfalls mit dem Status {@link Response.Status#INTERNAL_SERVER_ERROR}
     * und jeweils mit entsprechender Fehlermeldung zurückgegeben wird
     *
     * @param function Die auszuführende {@link ExFunction},
     *                 aus deren Resultat die {@link Response} erzeugt werden soll
     * @param t        Das Argument für den Funktionsparameter
     * @param <R>      Der Typ des Funktionsergebnisses
     * @param <T>      Der Typ des Funktionsparameters
     * @return Die erzeugte Response
     */
    public static <R, T> Response create(ExFunction<R, T> function, T t) {
        LOGGER.debug("Creating response for function {}", function);
        var response = Response.noContent();
        try {
            LOGGER.trace("Applying function {} with argument {}", function, t);
            R entity = function.apply(t);
            LOGGER.trace("Function returned {}", entity);
            if (!(function instanceof ExConsumer)) {
                LOGGER.trace("Function is not an ExConsumer, setting entity");
                response = Response.ok(entity);
            } else {
                LOGGER.trace("Function is an ExConsumer, setting no entity");
            }
        } catch (SQLException e) {
            LOGGER.warn("Database communication error", e);
            SQLExceptionHandler.handle(e, LOGGER);
            Throwable entity = new Throwable("An error during database communication occurred");
            response = Response.serverError().entity(entity);
        } catch (IllegalArgumentException e) {
            LOGGER.warn("Client request had bad arguments", e);
            response = Response.status(Response.Status.BAD_REQUEST).entity(e);
        } catch (RuntimeException e) {
            LOGGER.error("Unexpected exception was thrown", e);
            response = Response.serverError().entity(e);
        }
        var build = response.build();
        LOGGER.trace("Created response {}", build);
        return build;
    }

    /**
     * Convenience-Methode zum Erzeugen einer Response
     * als Resultat des Ausführens von {@link ExConsumer#accept(Object)}
     *
     * @param consumer Der auszuführende {@link ExConsumer}
     * @param t        Das Argument für den Funktionsparameter
     * @param <T>      Der Typ des Funktionsparameters
     * @return Die erzeugte Response
     */
    public static <T> Response create(ExConsumer<T> consumer, T t) {
        return ResponseFactory.<Void, T>create(consumer, t);
    }

    /**
     * Erzeugt eine {@link Response} mit dem Status {@link Response.Status#UNAUTHORIZED}
     * und einem entsprechenden Fehler als Entity
     *
     * @return Die erzeugte Response
     */
    public static Response createUnauthorized() {
        LOGGER.warn("Unauthorized request");
        Throwable entity = new IllegalCallerException("Insufficient privileges to perform the request");
        return Response.status(Response.Status.UNAUTHORIZED).entity(entity).build();
    }
}
