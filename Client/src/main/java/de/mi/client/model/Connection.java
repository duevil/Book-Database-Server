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
        return createBuilder().path("subfields").requestGET().read(new GenericType<>() {});
    }

    public List<Book> getBooks() throws WebApplicationException {
        return getBooks(null);
    }

    public List<Book> getBooks(BookFilter filter) throws WebApplicationException {
        var builder = createBuilder().path("books");
        var result = filter == null ? builder.requestGET() : builder.requestPOST(filter);
        return result.read(new GenericType<>() {});
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
