package de.mi.client.model;

import de.mi.common.ClientType;
import de.mi.common.ServerURI;
import jakarta.ws.rs.HttpMethod;
import jakarta.ws.rs.client.Client;
import jakarta.ws.rs.client.Entity;
import jakarta.ws.rs.client.WebTarget;
import jakarta.ws.rs.core.HttpHeaders;
import jakarta.ws.rs.core.MediaType;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.Optional;

/**
 * Klasse zum Erstellen und Ausführen einer Http-Request über ein {@link WebTarget}
 *
 * @author Malte Kasolowsky <code>m30114</code>
 */
class RequestBuilder {
    private static final Logger LOGGER = LogManager.getLogger(RequestBuilder.class);
    private final ClientType clientType;
    private WebTarget target;

    /**
     * Konstruktor; speichert den übergebenen {@link ClientType}
     * und erzeugt ein neues {@link WebTarget} aus dem übergebenen {@link Client}
     *
     * @param client     Der Client, mit welchem das WebTarget erzeugt werden soll
     * @param clientType Die Art des Clients, welche zur Autorisierung der Request weitergeleitet wird
     */
    public RequestBuilder(Client client, ClientType clientType) {
        LOGGER.trace("Creating new RequestBuilder for client type {}", clientType);
        this.clientType = clientType;
        target = client.target(ServerURI.uri());
    }

    /**
     * Hängt dem Target dieses Builders einen {@link WebTarget#path(String) Pfad} an
     *
     * @param path Der (Teil-)Pfad, über dem das target die Request ausführen soll
     * @return Die eigene Instanz des Builders
     */
    public RequestBuilder path(String path) {
        LOGGER.debug("Adding path {} to request", path);
        target = target.path(path);
        return this;
    }

    /**
     * Hängt dem Target des Builders ein {@link WebTarget#queryParam(String, Object...) Query-Paramter} an
     *
     * @param name   Der Name des Query-Parameters
     * @param values Der/Die Wert/-e des Query-Parameters
     * @return Die eigene Instanz des Builders
     */
    public RequestBuilder queryParam(String name, Object... values) {
        LOGGER.debug("Adding query parameter {} with values {} to request", name, values);
        target = target.queryParam(name, values);
        return this;
    }

    /**
     * Führt eine {@link jakarta.ws.rs.client.SyncInvoker#get()}-Anfrage aus
     *
     * @return Ein {@link RequestResult} mit dem Ergebnis der Anfrage
     */
    public RequestResult requestGET() {
        return createRequest(HttpMethod.GET, null);
    }


    /**
     * Führt eine {@link jakarta.ws.rs.client.SyncInvoker#put(Entity)}-Anfrage aus
     *
     * @param entity Ein Objekt, welches der Anfrage als {@link Entity#json(Object) JSON-Entity} mitgeben wird
     * @param <T>    Der Typ des Entity-Objekts
     */
    public <T> void requestPUT(T entity) {
        createRequest(HttpMethod.PUT, entity);
    }

    /**
     * Führt eine {@link jakarta.ws.rs.client.SyncInvoker#post(Entity)}-Anfrage aus
     *
     * @param entity Ein Objekt, welches der Anfrage als {@link Entity#json(Object) JSON-Entity} mitgeben wird
     * @param <T>    Der Typ des Entity-Objekts
     * @return Ein {@link RequestResult} mit dem Ergebnis der Anfrage
     */
    public <T> RequestResult requestPOST(T entity) {
        return createRequest(HttpMethod.POST, entity);
    }

    /**
     * Führt eine {@link jakarta.ws.rs.client.SyncInvoker#delete()}-Anfrage aus
     */
    public void requestDELETE() {
        createRequest(HttpMethod.DELETE, null);
    }

    /**
     * Führt eine Http-Anfrage aus und erstellt aus der
     * von der Anfrage zurückerhaltenen {@link jakarta.ws.rs.core.Response}
     * ein neues {@link RequestResult}
     *
     * @param method Der Bezeichner für die Http-Methode der Anfrage;
     *               muss einer der Methoden GET, PUT, POST oder DELETE sein
     * @param entity Ein Objekt, welches der Http-Methode als
     *               {@link Entity#json(Object) JSON-Entity} mitgeben werden soll;
     *               dieses darf nur bei den Methoden PUT und POST angegeben werden
     * @param <T>    Der Typ des Entity-Objekts
     * @return Das Ergebnis der Anfrage
     */
    private <T> RequestResult createRequest(String method, T entity) {
        var builder = target.request()
                .accept(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, clientType);

        LOGGER.debug("Sending {} request to {}", method, target.getUri());
        LOGGER.trace("With client type: {}", clientType);
        LOGGER.trace("With entity: {}", entity);

        var response = Optional.ofNullable(entity)
                .map(Entity::json)
                .map(jsonEntity -> switch (method) {
                    case HttpMethod.PUT -> builder.put(jsonEntity);
                    case HttpMethod.POST -> builder.post(jsonEntity);
                    default -> throw new IllegalArgumentException("illegal method");
                })
                .orElseGet(() -> switch (method) {
                    case HttpMethod.GET -> builder.get();
                    case HttpMethod.DELETE -> builder.delete();
                    default -> throw new IllegalArgumentException("illegal method");
                });

        LOGGER.debug("Got response: {}", response);
        LOGGER.trace("With status: {}", response.getStatus());
        LOGGER.trace("With entity: {}", response.getEntity());

        return new RequestResult(response);
    }
}
