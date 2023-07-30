package de.mi.server;

import de.mi.common.ServerURI;
import jakarta.ws.rs.ProcessingException;
import jakarta.ws.rs.core.Application;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.glassfish.grizzly.http.server.HttpServer;
import org.glassfish.jersey.grizzly2.httpserver.GrizzlyHttpServerFactory;
import org.glassfish.jersey.server.ResourceConfig;

/**
 * Klasse zum Starten des REST-Servers
 *
 * @author Malte Kasolowsky <code>m30114</code>
 */
final class Server {
    private static final Logger LOGGER = LogManager.getLogger(Server.class);
    private static HttpServer serverInstance;

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
        LOGGER.debug("Starting server with application class {}", applicationClass);
        ResourceConfig config = ResourceConfig.forApplicationClass(applicationClass);
        LOGGER.trace("Created ResourceConfig for application class {}: {}", applicationClass, config);
        LOGGER.debug("Server URI: {}", ServerURI.uri());
        try {
            serverInstance = GrizzlyHttpServerFactory.createHttpServer(ServerURI.uri(), config, true);
            LOGGER.trace("Created server instance: {}", serverInstance);
            // waitForShutdownConfirmation(); // not possible with docker
        } catch (ProcessingException /*| IOException*/ e) {
            LOGGER.fatal("Exception thrown during server start", e);
            throw new IllegalStateException("Exception thrown during server start", e);
        }
        if (serverInstance.isStarted()) LOGGER.info("Server started at {}", ServerURI.uri());
        else LOGGER.error("Server not started");
    }

    /**
     * Stoppt den REST-Server
     */
    public static void stop() {
        LOGGER.info("Stopping server...");
        if (serverInstance != null) {
            serverInstance.shutdownNow();
            if (!serverInstance.isStarted()) LOGGER.info("Server stopped");
            else LOGGER.error("Server not stopped");
        }
    }

    /**
     * Gibt eine Nachricht zur Bestätigung des Server-Stoppens in der Konsole aus
     * und wartet auf eine korrekte Eingabe
     *
     * @deprecated not possible with docker
     */
    @Deprecated(since = "2.0") // console input not possible with docker
    @SuppressWarnings("unused")
    private static void waitForShutdownConfirmation() {
        LOGGER.info("Waiting for server shutdown confirmation...");
        String input;
        do {
            input = ConsolePrompter.prompt("At any time, enter 'STOP' to shutdown the server...");
        } while (!"STOP".equals(input));
        LOGGER.info("Server stopped");
    }
}
