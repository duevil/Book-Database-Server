package de.mi.server;

import de.mi.server.rest.LiteratureRest;
import org.apache.logging.log4j.LogManager;

/**
 * Hauptklasse des Programms; dient dem Initialisieren der Datenbank und dem Starten des REST-Servers
 *
 * @author Malte Kasolowsky <code>m30114</code>
 */
public class Main {
    static {
        System.setProperty(
                "java.util.logging.manager",
                "org.apache.logging.log4j.jul.LogManager"
        ); // redirect java util logging to log4j2
    }

    /**
     * Initialisiert die Datenbank mittels {@link DBConnection#executeResourceScript(String) Ausführung}
     * einer SQL-Datei, welche die Basisschemata und -daten der Datenbank enthält,
     * und {@link Server#start(Class) startet} den REST-Server
     *
     * @param args nicht benutzt
     */
    public static void main(String[] args) {
        DBConnection.executeResourceScript("literature.sql");
        Server.start(LiteratureRest.APPLICATION);
        var hook = new Thread(() -> {
            LogManager.getLogger(Main.class).info("Received shutdown signal, stopping programm...");
            DBConnection.close();
            Server.stop();
        });
        hook.setDaemon(false);
        Runtime.getRuntime().addShutdownHook(hook);
    }
}
