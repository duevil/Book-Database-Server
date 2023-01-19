package de.mi.client.model;

import jakarta.ws.rs.WebApplicationException;
import jakarta.ws.rs.core.GenericType;
import jakarta.ws.rs.core.Response;

class RequestResult {
    private final Response response;

    RequestResult(Response response) {
        if (response.getStatusInfo().getFamily() == Response.Status.Family.SUCCESSFUL)
            this.response = response;
        else if (response.hasEntity())
            try (response) {
                throw new WebApplicationException(response.readEntity(Throwable.class), response);
            }
        else throw new WebApplicationException(response);
    }

    public <T> T read(GenericType<T> type) {
        try (response) {
            return response.readEntity(type);
        }
    }

    public <T> T read(Class<T> type) {
        try (response) {
            return response.readEntity(type);
        }
    }
}
