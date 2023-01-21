package de.mi.client.model;

import de.mi.common.Book;
import de.mi.common.BookFilter;
import de.mi.common.ClientType;
import de.mi.common.Subfield;
import jakarta.ws.rs.WebApplicationException;
import jakarta.ws.rs.client.Client;
import jakarta.ws.rs.client.ClientBuilder;
import jakarta.ws.rs.core.GenericType;

import java.util.List;
import java.util.Set;

class Connection {
    private final Client client = ClientBuilder.newClient();
    private final ClientType clientType;

    public Connection(ClientType clientType) {
        this.clientType = clientType;
    }

    public String getProgrammName() throws WebApplicationException {
        return String.format("%s [%s]", createBuilder().requestGET().read(String.class), clientType);
    }

    private RequestBuilder createBuilder() {
        return new RequestBuilder(client, clientType);
    }

    public Set<Subfield> getSubfields() throws WebApplicationException {
        return createBuilder().path("subfields").requestGET().read(new GenericType<>() {
        });
    }

    public List<Book> getBooks() throws WebApplicationException {
        return getBooks(null);
    }

    public List<Book> getBooks(BookFilter filter) throws WebApplicationException {
        final var bookBuilder = createBuilder().path("books");
        if (filter != null) {
            bookBuilder.path("filter")
                    .queryParam("title", filter.titleSearch())
                    .queryParam("author", filter.authorSearch())
                    .queryParam("min_year", filter.yearRange().min())
                    .queryParam("max_year", filter.yearRange().max())
                    .queryParam("min_pages", filter.pageRange().min())
                    .queryParam("max_pages", filter.pageRange().max())
                    .queryParam("min_rating", filter.ratingRange().min())
                    .queryParam("max_rating", filter.ratingRange().max())
                    .queryParam("subfield", filter.subfields().stream().map(Subfield::id).toArray());
        }
        return bookBuilder.requestGET().read(new GenericType<>() {
        });
    }

    public void updateBook(Book book) throws WebApplicationException {
        createBuilder().path("update").requestPUT(book);
    }

    public void createBook(Book book) throws WebApplicationException {
        createBuilder().path("create").requestPOST(book);
    }

    public void deleteBook(Book book) throws WebApplicationException {
        createBuilder().path("delete").queryParam("id", book.id()).requestDELETE();
    }
}
