package de.mi.model;

import jakarta.ws.rs.HttpMethod;
import jakarta.ws.rs.client.Client;
import jakarta.ws.rs.client.Entity;
import jakarta.ws.rs.client.WebTarget;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.Response;
import jakarta.ws.rs.core.UriBuilder;

import java.util.Optional;

class RequestBuilder {
    private final WebTarget target;

    public RequestBuilder(String host, int port, String namespace, Client client) {
        var uri = UriBuilder.newInstance()
                .scheme("http")
                .host(host)
                .port(port)
                .path(namespace)
                .build();
        target = client.target(uri);
    }

    private RequestBuilder(WebTarget target) {
        this.target = target;
    }

    public RequestBuilder path(String path) {
        return new RequestBuilder(target.path(path));
    }

    public RequestBuilder queryParam(String name, Object... values) {
        return new RequestBuilder(target.queryParam(name, values));
    }

    public RequestResult requestGET() {
        return createRequest(HttpMethod.GET, null);
    }

    public <T> RequestResult requestPUT(T entity) {
        return createRequest(HttpMethod.PUT, entity);
    }

    public <T> RequestResult requestPOST(T entity) {
        return createRequest(HttpMethod.POST, entity);
    }

    public RequestResult requestDELETE() {
        return createRequest(HttpMethod.DELETE, null);
    }

    private <T> RequestResult createRequest(String method, T entity) {
        var builder = target.request().accept(MediaType.APPLICATION_JSON);
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
}
