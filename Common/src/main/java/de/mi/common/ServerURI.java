package de.mi.common;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.ResourceBundle;

public final class ServerURI {
    private static final URI URI;

    static {
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

    private ServerURI() {
    }

    public static URI uri() {
        return URI;
    }
}
