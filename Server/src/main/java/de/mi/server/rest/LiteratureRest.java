package de.mi.server.rest;

import de.mi.common.Book;
import de.mi.common.BookFilter;
import de.mi.common.ClientType;
import de.mi.server.LiteratureQuery;
import de.mi.server.LiteratureUpdater;
import jakarta.ws.rs.Consumes;
import jakarta.ws.rs.DELETE;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.HeaderParam;
import jakarta.ws.rs.POST;
import jakarta.ws.rs.PUT;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.QueryParam;
import jakarta.ws.rs.core.Application;
import jakarta.ws.rs.core.Response;

import java.util.HashSet;
import java.util.Set;

import static jakarta.ws.rs.core.HttpHeaders.AUTHORIZATION;
import static jakarta.ws.rs.core.MediaType.APPLICATION_JSON;

/**
 * Klasse zum Bereitstellen der REST-Schnittstelle mit entsprechenden Verbindungspfaden
 *
 * @author Malte Kasolowsky <code>m30114</code>
 */
@Path("/")
public class LiteratureRest {

    /**
     * Referenz auf die interne {@link Application}-Klasse der Anwendung
     */
    public static final Class<? extends Application> APPLICATION = RestApplication.class;

    /**
     * Pfad zum Anfragen des Programm-Names
     *
     * @return Eine {@link Response}, welche den Programmnamen beinhaltet
     */
    @GET
    @Produces(APPLICATION_JSON)
    public Response getName() {
        return ResponseFactory.create(() -> "Informatik Fachliteratur");
    }

    /**
     * Pfad zum Anfragen der {@link de.mi.common.Subfield Teilgebiete} aus der Datenbank
     *
     * @return Eine {@link Response} mit den aus der Datenbank geladenen Teilgebieten
     */
    @GET
    @Path("subfields")
    @Produces(APPLICATION_JSON)
    public Response getSubfields() {
        return ResponseFactory.create(LiteratureQuery::getSubfields);
    }

    /**
     * Pfad zum Anfragen aller {@link Book Bücher} aus der Datenbank
     *
     * @return Eine {@link Response} mit den aus der Datenbank geladenen Büchern
     */
    @GET
    @Path("books")
    @Produces(APPLICATION_JSON)
    public Response queryBooks() {
        return queryBooks(null);
    }

    /**
     * Pfad zum Anfragen aller {@link Book Bücher} aus der Datenbank,
     * welche vom übergebenen {@link BookFilter} gefiltert wurden
     *
     * @param filter Der Filter zum Filtern der Bücher; ist dieser null, so werden alle Bücher geladen
     * @return Eine {@link Response} mit den aus der Datenbank geladenen, gefilterten Büchern
     */
    @POST
    @Path("books")
    @Produces(APPLICATION_JSON)
    @Consumes(APPLICATION_JSON)
    public Response queryBooks(BookFilter filter) {
        return filter == null
                ? ResponseFactory.create(LiteratureQuery::queryBooks)
                : ResponseFactory.<Set<Book>, BookFilter>create(LiteratureQuery::queryBooks, filter);
    }

    /**
     * Pfad zum Updaten eines {@link Book Buches}
     *
     * @param type Der {@link ClientType} der Anfrage
     * @param book Das zu updatende Buch
     * @return Eine {@link Response};
     * ist das zu updatende Buch nicht existent, so hat diese den Status {@link Response.Status#BAD_REQUEST},
     * ist der ClientType der Anfrage kein {@link ClientType#isMaster() Hauptnutzer},
     * so hat sie den Status {@link Response.Status#UNAUTHORIZED}
     */
    @PUT
    @Path("update")
    @Consumes(APPLICATION_JSON)
    public Response updateBook(@HeaderParam(AUTHORIZATION) ClientType type, Book book) {
        return type.isMaster()
                ? ResponseFactory.create(LiteratureUpdater::updateBook, book)
                : ResponseFactory.createUnauthorized();
    }

    /**
     * Pfad zum Erstellen eines {@link Book Buches}
     *
     * @param type Der {@link ClientType} der Anfrage
     * @param book Das zu erstellende Buch
     * @return Eine {@link Response};
     * ist das zu erstellende Buch bereits existent, so hat diese den Status {@link Response.Status#BAD_REQUEST},
     * ist der ClientType der Anfrage kein {@link ClientType#isMaster() Hauptnutzer},
     * so hat sie den Status {@link Response.Status#UNAUTHORIZED}
     */
    @POST
    @Path("create")
    @Consumes(APPLICATION_JSON)
    public Response createBook(@HeaderParam(AUTHORIZATION) ClientType type, Book book) {
        return type.isMaster()
                ? ResponseFactory.create(LiteratureUpdater::insertBook, book)
                : ResponseFactory.createUnauthorized();
    }

    /**
     * Pfad zum Löschen eines {@link Book Buches}
     *
     * @param type Der {@link ClientType} der Anfrage
     * @param id   Die ID des löschenden Buches
     * @return Eine {@link Response};
     * ist der ClientType der Anfrage kein {@link ClientType#isMaster() Hauptnutzer},
     * so hat sie den Status {@link Response.Status#UNAUTHORIZED}
     */
    @DELETE
    @Path("delete")
    public Response deleteBook(@HeaderParam(AUTHORIZATION) ClientType type, @QueryParam("id") int id) {
        return type.isMaster()
                ? ResponseFactory.create(LiteratureUpdater::deleteBook, id)
                : ResponseFactory.createUnauthorized();
    }

    /**
     * Interne {@link Application}-Klasse, welche ein statisches {@link Set} mit der Hauptklasse beinhaltet
     */
    private static class RestApplication extends Application {
        private static final Set<Class<?>> CLASSES = new HashSet<>();

        /**
         * Konstruktor; fügt die Hauptklasse dem internen Set hinzu
         */
        public RestApplication() {
            CLASSES.add(LiteratureRest.class);
        }

        /**
         * Gibt das Set zurück
         *
         * @return Das Set mit der Hauptklasse
         */
        @Override
        public Set<Class<?>> getClasses() {
            return CLASSES;
        }
    }
}
