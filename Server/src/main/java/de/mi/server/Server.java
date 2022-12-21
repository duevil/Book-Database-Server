package de.mi.server;

import de.mi.SimplePrompter;
import de.mi.common.ServerURI;
import jakarta.ws.rs.core.Application;
import org.glassfish.grizzly.http.server.HttpServer;
import org.glassfish.jersey.grizzly2.httpserver.GrizzlyHttpServerFactory;
import org.glassfish.jersey.server.ResourceConfig;

import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;

final class Server {
    private static final Logger LOGGER = Logger.getLogger("org.glassfish");

    private Server() {
    }

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

    private static void waitForShutdownConfirmation() {
        LOGGER.info("Waiting for server shutdown confirmation...");
        String input;
        do {
            input = SimplePrompter.getConsoleInput("Enter 'STOP' to shutdown the server...");
        } while (!"STOP".equals(input));
        LOGGER.info("Server stopped");
    }

}
