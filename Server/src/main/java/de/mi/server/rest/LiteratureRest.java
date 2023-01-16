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
import jakarta.ws.rs.core.HttpHeaders;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.Response;

import java.util.HashSet;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

@Path("/")
public class LiteratureRest {

    public static final Class<? extends Application> APPLICATION = RestApplication.class;

    @GET
    @Produces(MediaType.APPLICATION_JSON)
    public Response getName() {
        return ResponseFactory.create(() -> "Informatik Fachliteratur");
    }

    @GET
    @Path("subfields")
    @Produces(MediaType.APPLICATION_JSON)
    public Response getSubfields() {
        return ResponseFactory.create(LiteratureQuery::getSubfields);
    }

    @GET
    @Path("books")
    @Produces(MediaType.APPLICATION_JSON)
    public Response getAllBooks() {
        return getBooks(null);
    }

    @POST
    @Path("books")
    @Produces(MediaType.APPLICATION_JSON)
    @Consumes(MediaType.APPLICATION_JSON)
    public Response getBooks(BookFilter filter) {
        return filter == null
                ? ResponseFactory.create(LiteratureQuery::queryBooks)
                : ResponseFactory.<Set<Book>, BookFilter>create(LiteratureQuery::queryBooks, filter);
    }

    @GET
    @Path("books/filter")
    @Produces(MediaType.APPLICATION_JSON)
    public Response getBooks(
            @QueryParam("title") String titleSearch,
            @QueryParam("author") String authorSearch,
            @QueryParam("min_year") Integer minYear,
            @QueryParam("max_year") Integer maxYear,
            @QueryParam("min_pages") Integer minPages,
            @QueryParam("max_pages") Integer maxPages,
            @QueryParam("subfield") Set<Integer> subfieldIDs
    ) {
        BookFilter filter = BookFilter.builder()
                .searchTitle(titleSearch)
                .searchAuthor(authorSearch)
                .yearRange(
                        Optional.ofNullable(minYear).orElse(Book.DEFAULT_YEAR_RANGE.min()),
                        Optional.ofNullable(maxYear).orElse(Book.DEFAULT_YEAR_RANGE.max()))
                .pageRange(
                        Optional.ofNullable(minPages).orElse(Book.DEFAULT_PAGE_RANGE.min()),
                        Optional.ofNullable(maxPages).orElse(Book.DEFAULT_PAGE_RANGE.max()))
                .subfields(LiteratureQuery.getSubfields()
                        .stream()
                        .filter(s -> Optional.ofNullable(subfieldIDs).orElse(Set.of()).contains(s.id()))
                        .collect(Collectors.toSet()))
                .build();
        return getBooks(filter);
    }

    @PUT
    @Path("update")
    @Consumes(MediaType.APPLICATION_JSON)
    public Response updateBook(
            @HeaderParam(HttpHeaders.AUTHORIZATION) ClientType type,
            Book book
    ) {
        return switch (type) {
            case BASIC -> ResponseFactory.createUnauthorized();
            case MASTER -> ResponseFactory.create(LiteratureUpdater::updateBook, book);
        };
    }

    @POST
    @Path("create")
    @Consumes(MediaType.APPLICATION_JSON)
    public Response createBook(
            @HeaderParam(HttpHeaders.AUTHORIZATION) ClientType type,
            Book book
    ) {
        return switch (type) {
            case BASIC -> ResponseFactory.createUnauthorized();
            case MASTER -> ResponseFactory.create(LiteratureUpdater::insertBook, book);
        };
    }

    @DELETE
    @Path("delete")
    public Response deleteBook(
            @HeaderParam(HttpHeaders.AUTHORIZATION) ClientType type,
            @QueryParam("id") int bookID
    ) {
        return switch (type) {
            case BASIC -> ResponseFactory.createUnauthorized();
            case MASTER -> ResponseFactory.create(LiteratureUpdater::deleteBook, bookID);
        };
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
