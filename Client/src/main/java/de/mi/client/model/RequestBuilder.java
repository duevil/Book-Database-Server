package de.mi.client.model;

import de.mi.common.ClientType;
import de.mi.common.ServerURI;
import jakarta.ws.rs.HttpMethod;
import jakarta.ws.rs.client.Client;
import jakarta.ws.rs.client.Entity;
import jakarta.ws.rs.client.Invocation;
import jakarta.ws.rs.client.WebTarget;
import jakarta.ws.rs.core.HttpHeaders;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.Response;

import java.util.Optional;

class RequestBuilder {
    private final WebTarget target;
    private final ClientType clientType;

    public RequestBuilder(Client client, ClientType clientType) {
        this.clientType = clientType;
        target = client.target(ServerURI.uri());
    }

    private RequestBuilder(WebTarget target, ClientType clientType) {
        this.target = target;
        this.clientType = clientType;
    }

    public RequestBuilder path(String path) {
        return new RequestBuilder(target.path(path), clientType);
    }

    public RequestBuilder queryParam(String name, Object... values) {
        return new RequestBuilder(target.queryParam(name, values), clientType);
    }

    public RequestResult requestGET() throws IllegalArgumentException {
        return createRequest(HttpMethod.GET, null);
    }

    public <T> RequestResult requestPUT(T entity) throws IllegalArgumentException {
        return createRequest(HttpMethod.PUT, entity);
    }

    public <T> RequestResult requestPOST(T entity) throws IllegalArgumentException {
        return createRequest(HttpMethod.POST, entity);
    }

    public RequestResult requestDELETE() throws IllegalArgumentException {
        return createRequest(HttpMethod.DELETE, null);
    }

    private <T> RequestResult createRequest(String method, T entity) throws IllegalArgumentException {
        var builder = createRequestBuilder();
        Response response = Optional.ofNullable(entity)
                .map(Entity::json)
                .map(tEntity -> switch (method) {
                    case HttpMethod.PUT -> builder.put(tEntity);
                    case HttpMethod.POST -> builder.post(tEntity);
                    default -> throw new IllegalArgumentException("illegal method");
                })
                .orElseGet(() -> switch (method) {
                    case HttpMethod.GET -> builder.get();
                    case HttpMethod.DELETE -> builder.delete();
                    default -> throw new IllegalArgumentException("illegal method");
                });
        return new RequestResult(response);
    }

    private Invocation.Builder createRequestBuilder() {
        return target.request()
                .accept(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, clientType);
    }
}
