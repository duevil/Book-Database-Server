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

@Path("/")
public class LiteratureRest {

    public static final Class<? extends Application> APPLICATION = RestApplication.class;

    @GET
    @Produces(APPLICATION_JSON)
    public Response getName() {
        return ResponseFactory.create(() -> "Informatik Fachliteratur");
    }

    @GET
    @Path("subfields")
    @Produces(APPLICATION_JSON)
    public Response getSubfields() {
        return ResponseFactory.create(LiteratureQuery::getSubfields);
    }

    @GET
    @Path("books")
    @Produces(APPLICATION_JSON)
    public Response queryBooks() {
        return queryBooks(null);
    }

    @POST
    @Path("books")
    @Produces(APPLICATION_JSON)
    @Consumes(APPLICATION_JSON)
    public Response queryBooks(BookFilter filter) {
        return filter == null
                ? ResponseFactory.create(LiteratureQuery::queryBooks)
                : ResponseFactory.<Set<Book>, BookFilter>create(LiteratureQuery::queryBooks, filter);
    }

    @PUT
    @Path("update")
    @Consumes(APPLICATION_JSON)
    public Response updateBook(@HeaderParam(AUTHORIZATION) ClientType type, Book book) {
        return type.isMaster()
                ? ResponseFactory.create(LiteratureUpdater::updateBook, book)
                : ResponseFactory.createUnauthorized();
    }

    @POST
    @Path("create")
    @Consumes(APPLICATION_JSON)
    public Response createBook(@HeaderParam(AUTHORIZATION) ClientType type, Book book) {
        return type.isMaster()
                ? ResponseFactory.create(LiteratureUpdater::insertBook, book)
                : ResponseFactory.createUnauthorized();
    }

    @DELETE
    @Path("delete")
    public Response deleteBook(@HeaderParam(AUTHORIZATION) ClientType type, @QueryParam("id") int id) {
        return type.isMaster()
                ? ResponseFactory.create(LiteratureUpdater::deleteBook, id)
                : ResponseFactory.createUnauthorized();
    }

    private static class RestApplication extends Application {
        private static final Set<Class<?>> CLASSES = new HashSet<>();

        public RestApplication() {
            CLASSES.add(LiteratureRest.class);
        }

        @Override
        public Set<Class<?>> getClasses() {
            return CLASSES;
        }
    }
}
