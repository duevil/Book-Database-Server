package de.mi.server;

import de.mi.server.rest.LiteratureRest;

public class Main {
    public static void main(String[] args) {
        DBConnection.executeResourceScript("literature.sql");
        Server.start(LiteratureRest.APPLICATION);
        // start will block the programm
        // thus close will only be called when the server has been stoppen
        DBConnection.close();
    }
}
