package de.mi.client.model;

import de.mi.common.Book;
import de.mi.common.BookFilter;
import de.mi.common.ClientType;
import de.mi.common.Subfield;
import jakarta.ws.rs.WebApplicationException;
import jakarta.ws.rs.client.ClientBuilder;
import jakarta.ws.rs.core.GenericType;

import java.util.List;
import java.util.Optional;
import java.util.Set;

class Connection {
    private final ClientType clientType;

    public Connection(ClientType clientType) {

        this.clientType = clientType;
    }

    public String getProgrammName() throws WebApplicationException {
        return String.format("%s [%s]",
                getBuilder().requestGET().read(String.class),
                clientType);
    }

    public Set<Subfield> getSubfields() throws WebApplicationException {
        return getBuilder().path("subfields").requestGET().read(new GenericType<>() {
        });
    }

    public List<Book> getBooks() throws WebApplicationException {
        return getBooks(null);
    }

    public List<Book> getBooks(BookFilter filter) throws WebApplicationException {
        final var bookBuilder = getBuilder().path("books");
        var books = Optional.ofNullable(filter)
                //.map(bookBuilder::requestPUT) // not working :(
                .map(bookFilter -> bookBuilder.path("filter")
                        .queryParam("title", bookFilter.titleSearch())
                        .queryParam("author", bookFilter.authorSearch())
                        .queryParam("min_year", bookFilter.yearRange().min())
                        .queryParam("max_year", bookFilter.yearRange().max())
                        .queryParam("min_pages", bookFilter.pageRange().min())
                        .queryParam("max_pages", bookFilter.pageRange().max())
                        .queryParam("min_rating", bookFilter.ratingRange().min())
                        .queryParam("max_rating", bookFilter.ratingRange().max())
                        .queryParam("subfield", bookFilter.subfields().stream().map(Subfield::id).toArray())
                        .requestGET())
                .orElseGet(bookBuilder::requestGET);
        return books.read(new GenericType<>() {
        });
    }

    public void updateBook(Book book) throws WebApplicationException {
        getBuilder().path("update").requestPUT(book);
    }

    public void createBook(Book book) throws WebApplicationException {
        getBuilder().path("create").requestPOST(book);
    }

    public void deleteBook(Book book) throws WebApplicationException {
        getBuilder().path("delete").queryParam("id", book.id()).requestDELETE();
    }

    private RequestBuilder getBuilder() {
        return new RequestBuilder(ClientBuilder.newClient(), clientType);
    }
}
