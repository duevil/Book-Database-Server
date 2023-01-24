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

/**
 * Klasse zum Erstellen von Anfragen an den Server zum Laden und Manipulieren von Literatur-Daten
 *
 * @author Malte Kasolowsky <code>m30114</code>
 */
class Connection {
    private final Client client = ClientBuilder.newClient();
    private final ClientType clientType;

    /**
     * Konstruktor; speichert den {@link ClientType} der Verbindung
     *
     * @param clientType Der Art des Clients
     */
    public Connection(ClientType clientType) {
        this.clientType = clientType;
    }

    /**
     * Lädt den Programmnamen vom Server
     *
     * @return Den Namen des Programms, welcher vom Server geladen wurde
     * @throws WebApplicationException Wenn die Anfrage nicht erfolgreich war
     */
    public String getProgrammName() throws WebApplicationException {
        return String.format("%s [%s]", createBuilder().requestGET().read(String.class), clientType);
    }

    /**
     * Lädt die {@link Subfield Teilgebiete} vom Server
     *
     * @return Die Teilgebiete, welche vom Server geladen wurden
     * @throws WebApplicationException Wenn die Anfrage nicht erfolgreich war
     */
    public Set<Subfield> getSubfields() throws WebApplicationException {
        return createBuilder().path("subfields").requestGET().read(new GenericType<>() {});
    }

    /**
     * Lädt alle {@link Book Bücher} vom Server
     *
     * @return Alle geladenen Bücher, welche vom Server geladen wurden
     * @throws WebApplicationException Wenn die Anfrage nicht erfolgreich war
     */
    public List<Book> getBooks() throws WebApplicationException {
        return getBooks(null);
    }

    /**
     * Lädt die {@link Book Bücher} vom Server, welche durch einen {@link BookFilter} gefiltert wurden
     *
     * @param filter Der Filter zum Filtern der zu ladenden Bücher
     * @return Die gefilterten Bücher, welche vom Server geladen wurden
     * @throws WebApplicationException Wenn die Anfrage nicht erfolgreich war
     */
    public List<Book> getBooks(BookFilter filter) throws WebApplicationException {
        var builder = createBuilder().path("books");
        var result = filter == null ? builder.requestGET() : builder.requestPOST(filter);
        return result.read(new GenericType<>() {});
    }

    /**
     * Updated ein {@link Book} auf dem Server
     *
     * @param book Das zu updatende Buch
     * @throws WebApplicationException Wenn die Anfrage nicht erfolgreich war
     */
    public void updateBook(Book book) throws WebApplicationException {
        createBuilder().path("update").requestPUT(book);
    }

    /**
     * Löscht ein {@link Book} auf dem Server
     *
     * @param book Das zu löschende Buch
     * @throws WebApplicationException Wenn die Anfrage nicht erfolgreich war
     */
    public void createBook(Book book) throws WebApplicationException {
        createBuilder().path("create").requestPOST(book);
    }

    /**
     * Erstellt ein neues Buch auf dem Server
     *
     * @param book Das Buch, welches auf dem Server erstellt werden soll
     * @throws WebApplicationException Wenn die Anfrage nicht erfolgreich war
     */
    public void deleteBook(Book book) throws WebApplicationException {
        createBuilder().path("delete").queryParam("id", book.id()).requestDELETE();
    }

    /**
     * Erzeugt einen neuen {@link RequestBuilder}
     *
     * @return Eine neue Instanz eines RequestBuilders
     */
    private RequestBuilder createBuilder() {
        return new RequestBuilder(client, clientType);
    }
}
