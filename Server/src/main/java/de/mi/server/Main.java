package de.mi.server;

import de.mi.server.rest.LiteratureRest;

/**
 * Hauptklasse des Programms; dient dem Initialisieren der Datenbank und dem Starten des REST-Servers
 *
 * @author Malte Kasolowsky <code>m30114</code>
 */
public class Main {
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
        // start will block the programm
        // thus close will only be called when the server has been stoppen
        DBConnection.close();
    }
}
