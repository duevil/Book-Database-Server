package de.mi.client.model;

import jakarta.ws.rs.WebApplicationException;
import jakarta.ws.rs.core.GenericType;
import jakarta.ws.rs.core.Response;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * Klasse, welche eine {@link Response} ummantelt und diese prüft und ausließt
 *
 * @author Malte Kasolowsky <code>m30114</code>
 */
class RequestResult {
    private static final Logger LOGGER = LogManager.getLogger(RequestResult.class);
    private final Response response;

    /**
     * Konstruktor; prüft, ob eine Http-Anfrage erfolgreich war, indem die geprüft wird,
     * ob die übergebene {@link Response} den Status
     * der Gruppe {@link Response.Status.Family#SUCCESSFUL} hat und wirft andernfalls eine entsprechende
     * Ausnahme, wobei diese, sollte die Response einen {@link Throwable Grund}
     * für den Fehler mitgegeben bekommen haben, diesen Grund als Ursache übergeben bekommt
     *
     * @param response Die zu prüfende und speichernde Response, sofern die Anfrage erfolgreich war
     * @throws WebApplicationException Wenn die Anfrage nicht erfolgreich war
     */
    RequestResult(Response response) throws WebApplicationException {
        LOGGER.trace("Creating new RequestResult for response {}", response);
        if (response.getStatusInfo().getFamily() == Response.Status.Family.SUCCESSFUL) {
            LOGGER.debug("Request was successful");
            this.response = response;
        } else if (response.hasEntity()) {
            try (response) {
                var cause = response.readEntity(Throwable.class);
                LOGGER.warn("Request was not successful: {}", cause.getMessage());
                throw new WebApplicationException(cause, response);
            }
        } else {
            LOGGER.warn("Request was not successful: {}", response.getStatusInfo().getReasonPhrase());
            throw new WebApplicationException(response);
        }
    }

    /**
     * Liest die gespeicherte {@link Response} aus,
     * wobei diese nach dem Auslesen {@link Response#close() geschlossen} wird
     *
     * @param type Der {@link GenericType Datentyp}, der ausgelesen werden soll
     * @param <T>  Der Datentyp, der ausgelesen werden soll
     * @return Die Rückgabe von {@link Response#readEntity(GenericType)}
     */
    public <T> T read(GenericType<T> type) {
        try (response) {
            LOGGER.debug("Reading entity of type {}", type);
            return response.readEntity(type);
        }
    }

    /**
     * Liest die gespeicherte {@link Response} aus,
     * wobei diese nach dem Auslesen {@link Response#close() geschlossen} wird
     *
     * @param type Der Datentyp, der ausgelesen werden soll
     * @param <T>  Der Datentyp, der ausgelesen werden soll
     * @return Die Rückgabe von {@link Response#readEntity(Class)}
     */
    public <T> T read(Class<T> type) {
        try (response) {
            LOGGER.debug("Reading entity of type {}", type);
            return response.readEntity(type);
        }
    }
}
