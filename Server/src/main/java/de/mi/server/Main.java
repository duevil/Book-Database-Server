package de.mi.server;

import de.mi.db.DBConnection;

public class Main {
    public static void main(String[] args) {
        DBConnection.executeResourceScript("literature-tables.sql");
        DBConnection.executeResourceScript("literature-data.sql");
        DBConnection.executeResourceScript("subfields-data.sql");
        Server.start(LiteratureRest.APPLICATION);
        // start will block the programm
        // thus close will only be called when the server has been stoppen
        DBConnection.close();
    }
}
