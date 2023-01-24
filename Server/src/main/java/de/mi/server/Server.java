package de.mi.server;

import de.mi.common.ServerURI;
import jakarta.ws.rs.core.Application;
import org.glassfish.grizzly.http.server.HttpServer;
import org.glassfish.jersey.grizzly2.httpserver.GrizzlyHttpServerFactory;
import org.glassfish.jersey.server.ResourceConfig;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.Charset;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Klasse zum Starten des REST-Servers
 */
final class Server {
    private static final Logger LOGGER = Logger.getLogger("org.glassfish");

    /**
     * Privater Konstruktor; eine Erzeugung einer Klassen-Instanz ist nicht nötig
     */
    private Server() {
    }

    /**
     * Startet den REST-Server mit der {@link ServerURI} und der übergebenen REST-{@link Application}-Klasse
     * und wartet nach dem Starten auf eine {@link Server#waitForShutdownConfirmation() Konosleneingabe},
     * nach welcher der Server gestoppt wird
     *
     * @param applicationClass Eine Klasse, welche vom Server für die REST-Schnittstelle genutzt werden soll
     */
    public static void start(Class<? extends Application> applicationClass) {
        ResourceConfig config = ResourceConfig.forApplicationClass(applicationClass);
        HttpServer server = GrizzlyHttpServerFactory.createHttpServer(ServerURI.uri(), config);
        try {
            if (!server.isStarted()) server.start();
            waitForShutdownConfirmation();
        } catch (IOException e) {
            LOGGER.log(Level.SEVERE, "Exception thrown during server start", e);
        } finally {
            server.shutdown();
        }
    }

    /**
     * Gibt eine Nachricht zur Bestätigung des Server-Stoppens in der Konsole aus
     * und wartet auf eine korrekte Eingabe
     */
    private static void waitForShutdownConfirmation() {
        LOGGER.info("Waiting for server shutdown confirmation...");
        String input;
        do {
            System.out.println("\u001B[34mEnter 'STOP' to shutdown the server...\u001B[0m");
            try (var os = new ByteArrayOutputStream()) {
                do os.write(System.in.read());
                while (System.in.available() > 0);
                input = os.toString(Charset.defaultCharset()).strip();
            } catch (IOException e) {
                throw new UncheckedIOException(e);
            }
        } while (!"STOP".equals(input));
        LOGGER.info("Server stopped");
    }
}
