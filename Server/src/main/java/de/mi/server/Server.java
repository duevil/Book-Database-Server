package de.mi.server;

import jakarta.ws.rs.core.Application;
import jakarta.ws.rs.core.UriBuilder;
import org.glassfish.grizzly.http.server.HttpServer;
import org.glassfish.jersey.grizzly2.httpserver.GrizzlyHttpServerFactory;
import org.glassfish.jersey.server.ResourceConfig;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.net.URI;
import java.nio.charset.Charset;
import java.util.logging.Level;
import java.util.logging.Logger;

final class Server {
    private static final Logger LOGGER = Logger.getLogger("org.glassfish");

    private Server() {
    }

    public static void start(
            String host,
            int port,
            String namespace,
            Class<? extends Application> applicationClass
    ) {
        URI uri = UriBuilder.fromUri("https://{h}:{p}/{n}").build(host, port, namespace);
        ResourceConfig config = ResourceConfig.forApplicationClass(applicationClass);
        HttpServer server = GrizzlyHttpServerFactory.createHttpServer(uri, config);
        try {
            if (!server.isStarted()) server.start();
            waitForShutdownConfirmation();
        } catch (IOException e) {
            LOGGER.log(Level.SEVERE, "Exception thrown during server start", e);
        } finally {
            server.shutdown();
        }
    }

    private static void waitForShutdownConfirmation() throws IOException {
        String input;
        do {
            LOGGER.info("Enter 'STOP' to shutdown the server... ");
            try (var os = new ByteArrayOutputStream()) {
                do os.write(System.in.read());
                while (System.in.available() > 0);
                input = os.toString(Charset.defaultCharset()).strip();
            }
        } while (!"STOP".equals(input));
    }
}
