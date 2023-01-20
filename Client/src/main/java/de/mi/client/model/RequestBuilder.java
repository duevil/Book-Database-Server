package de.mi.client.model;

import de.mi.common.ClientType;
import de.mi.common.ServerURI;
import jakarta.ws.rs.HttpMethod;
import jakarta.ws.rs.client.Client;
import jakarta.ws.rs.client.Entity;
import jakarta.ws.rs.client.WebTarget;
import jakarta.ws.rs.core.HttpHeaders;
import jakarta.ws.rs.core.MediaType;

import java.util.Optional;

class RequestBuilder {
    private final ClientType clientType;
    private WebTarget target;

    public RequestBuilder(Client client, ClientType clientType) {
        this.clientType = clientType;
        target = client.target(ServerURI.uri());
    }

    public RequestBuilder path(String path) {
        target = target.path(path);
        return this;
    }

    public RequestBuilder queryParam(String name, Object... values) {
        target = target.queryParam(name, values);
        return this;
    }

    public RequestResult requestGET() throws IllegalArgumentException {
        return createRequest(HttpMethod.GET, null);
    }

    public <T> void requestPUT(T entity) throws IllegalArgumentException {
        createRequest(HttpMethod.PUT, entity);
    }

    public <T> void requestPOST(T entity) throws IllegalArgumentException {
        createRequest(HttpMethod.POST, entity);
    }

    public void requestDELETE() throws IllegalArgumentException {
        createRequest(HttpMethod.DELETE, null);
    }

    private <T> RequestResult createRequest(String method, T entity) throws IllegalArgumentException {
        var builder = target.request()
                .accept(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, clientType);

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

        return new RequestResult(response);
    }
}
