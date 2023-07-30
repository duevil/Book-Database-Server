package de.mi.common;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.ResourceBundle;

/**
 * Utility-Klasse für systemweiten Zugriff auf die {@link URI}, über die die Client-Server-Kommunikation läuft
 *
 * @author Malte Kasolowsky <code>m30114</code>
 */
public final class ServerURI {
    private static final URI URI;

    static {
        // lädt die URI-Daten beim Laden der Klasse aus den properties-Ressourcen
        // und erzeugt aus diesen die Server-URI
        try {
            ResourceBundle bundle = ResourceBundle.getBundle("connection");
            URI = new URI(
                    "http",
                    null,
                    bundle.getString("host"),
                    Integer.parseInt(bundle.getString("port")),
                    '/' + bundle.getString("path"),
                    null,
                    null
            );
        } catch (URISyntaxException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    /**
     * Privater Konstruktor; eine Erzeugung einer Klassen-Instanz ist nicht nötig
     */
    private ServerURI() {
    }

    /**
     * Gibt die erzeugte {@link URI} zurück
     *
     * @return Die Server-URI
     */
    public static URI uri() {
        return URI;
    }
}
