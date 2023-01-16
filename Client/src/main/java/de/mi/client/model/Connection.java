package de.mi.client.model;

import de.mi.common.Book;
import de.mi.common.BookFilter;
import de.mi.common.ClientType;
import de.mi.common.Subfield;
import jakarta.ws.rs.client.ClientBuilder;
import jakarta.ws.rs.core.GenericType;

import java.util.List;
import java.util.Optional;
import java.util.Set;

public class Connection {
    private final RequestBuilder builder;
    private final ClientType clientType;

    public Connection(ClientType clientType) {
        this.clientType = clientType;
        builder = new RequestBuilder(ClientBuilder.newClient(), clientType);
    }

    public ClientType getClientType() {
        return clientType;
    }

    public String getProgrammName() {
        return builder.requestGET().read(String.class).orElse("null");
    }

    public Optional<Set<Subfield>> getSubfields() {
        return builder.path("subfields").requestGET().read(new GenericType<>() {
        });
    }

    public Optional<List<Book>> getBooks() {
        return getBooks(null);
    }

    public Optional<List<Book>> getBooks(BookFilter filter) {
        final var bookBuilder = builder.path("books");
        var books = Optional.ofNullable(filter)
                //.map(bookBuilder::requestPUT) // not working :(
                .map(bookFilter -> bookBuilder.path("filter")
                        .queryParam("title", bookFilter.titleSearch().orElse(""))
                        .queryParam("author", bookFilter.authorSearch().orElse(""))
                        .queryParam("min_year", bookFilter.yearRange().min())
                        .queryParam("max_year", bookFilter.yearRange().max())
                        .queryParam("min_pages", bookFilter.pageRange().min())
                        .queryParam("max_pages", bookFilter.pageRange().max())
                        .queryParam("subfield", bookFilter.subfields().stream().map(Subfield::id).toArray())
                        .requestGET())
                .orElseGet(bookBuilder::requestGET);
        return books.read(new GenericType<>() {
        });
    }

    public boolean updateBook(Book book) {
        return builder.path("update").requestPUT(book).success();
    }

    public boolean createBook(Book book) {
        return builder.path("create").requestPOST(book).success();
    }

    public boolean deleteBook(Book book) {
        return builder.path("delete").queryParam("id", book.id()).requestDELETE().success();
    }
}
