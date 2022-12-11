package de.mi.server;

import de.mi.common.Book;
import de.mi.common.BookFilter;
import de.mi.db.LiteratureQuery;
import de.mi.db.LiteratureUpdater;
import jakarta.ws.rs.Consumes;
import jakarta.ws.rs.DELETE;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.HttpMethod;
import jakarta.ws.rs.POST;
import jakarta.ws.rs.PUT;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.QueryParam;
import jakarta.ws.rs.core.Application;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.Response;

import java.sql.SQLException;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.logging.Logger;

@Path("/")
public class LiteratureRest {
    private static final Logger LOGGER = Logger.getLogger("org.glassfish");

    public static Class<? extends Application> getApplicationClass() {
        return RestApplication.class;
    }

    private static Response bookManipulation(Book book, String method) {
        try {
            switch (method) {
                case HttpMethod.PUT -> LiteratureUpdater.updateBook(book);
                case HttpMethod.POST -> LiteratureUpdater.insertBook(book);
                case HttpMethod.DELETE -> LiteratureUpdater.deleteBook(book);
                default -> { /* do nothing */ }
            }
            return Response.noContent().build();
        } catch (SQLException e) {
            LOGGER.severe(e::toString);
            return Response.serverError().build();
        } catch (IllegalArgumentException e) {
            LOGGER.info(e::toString);
            return Response.status(Response.Status.BAD_REQUEST).build();
        }
    }

    @GET
    @Produces(MediaType.TEXT_PLAIN)
    public Response getName() {
        return Response.ok("Informatik Fachliteratur").build();
    }

    @GET
    @Path("subfields")
    @Produces(MediaType.APPLICATION_JSON)
    public Response getSubfields() {
        return Response.ok(LiteratureQuery.getSubfields()).build();
    }

    @GET
    @Path("books/all")
    @Produces(MediaType.APPLICATION_JSON)
    public Response getAllBooks() {
        return getBooks(null);
    }

    @POST
    @Path("books")
    @Produces(MediaType.APPLICATION_JSON)
    @Consumes(MediaType.APPLICATION_JSON)
    public Response getBooks(BookFilter filter) {
        try {
            Set<Book> books = filter != null
                    ? LiteratureQuery.queryBooks(filter)
                    : LiteratureQuery.queryBooks();
            return Response.ok(books).build();
        } catch (SQLException e) {
            LOGGER.severe(e::toString);
            return Response.serverError().build();
        }
    }

    @GET
    @Path("books")
    @Produces(MediaType.APPLICATION_JSON)
    public Response getBooks(
            @QueryParam("title") String titleSearch,
            @QueryParam("author") String authorSearch,
            @QueryParam("min_year") Integer minYear,
            @QueryParam("max_year") Integer maxYear,
            @QueryParam("min_pages") Integer minPages,
            @QueryParam("max_pages") Integer maxPages,
            @QueryParam("subfield") List<Integer> subfieldIDs
    ) {
        BookFilter filter = BookFilter.builder()
                .searchTitle(titleSearch)
                .searchAuthor(authorSearch)
                .yearRange(
                        Optional.ofNullable(minYear).orElse(BookFilter.DEFAULT_YEAR_RANGE.min()),
                        Optional.ofNullable(maxYear).orElse(BookFilter.DEFAULT_YEAR_RANGE.max()))
                .pageRange(
                        Optional.ofNullable(minPages).orElse(BookFilter.DEFAULT_PAGE_RANGE.min()),
                        Optional.ofNullable(maxPages).orElse(BookFilter.DEFAULT_PAGE_RANGE.max()))
                .subfields(subfieldIDs)
                .build();
        return getBooks(filter);
    }

    @PUT
    @Path("change")
    @Consumes(MediaType.APPLICATION_JSON)
    public Response changeBook(Book book) {
        return bookManipulation(book, HttpMethod.PUT);
    }

    @POST
    @Path("add")
    @Consumes(MediaType.APPLICATION_JSON)
    public Response addBook(Book book) {
        return bookManipulation(book, HttpMethod.POST);
    }

    @DELETE
    @Path("remove")
    @Consumes(MediaType.APPLICATION_JSON)
    public Response deleteBook(Book book) {
        return bookManipulation(book, HttpMethod.DELETE);
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
