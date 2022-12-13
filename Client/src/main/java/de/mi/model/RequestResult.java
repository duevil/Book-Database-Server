package de.mi.model;

import jakarta.ws.rs.ClientErrorException;
import jakarta.ws.rs.ServerErrorException;
import jakarta.ws.rs.WebApplicationException;
import jakarta.ws.rs.core.GenericType;
import jakarta.ws.rs.core.Response;

import java.util.Optional;

class RequestResult {
    private final Response response;

    RequestResult(Response response) {
        this.response = response;
    }

    private static boolean checkResponse(Response response) throws WebApplicationException {
        return switch (response.getStatusInfo().getFamily()) {
            case SUCCESSFUL -> true;
            case CLIENT_ERROR -> throw new ClientErrorException(response);
            case SERVER_ERROR -> throw new ServerErrorException(response);
            default -> false;
        };
    }

    public boolean success() throws WebApplicationException {
        return checkResponse(response);
    }

    public <T> Optional<T> read(GenericType<T> type) throws WebApplicationException {
        return Optional.of(response)
                .filter(RequestResult::checkResponse)
                .flatMap(r -> Optional.ofNullable(type).map(r::readEntity));
    }

    public <T> Optional<T> read(Class<T> type) throws WebApplicationException {
        return Optional.of(response)
                .filter(RequestResult::checkResponse)
                .flatMap(r -> Optional.ofNullable(type).map(r::readEntity));
    }
}
