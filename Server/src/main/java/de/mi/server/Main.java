package de.mi.server;

import de.mi.db.DBConnection;

import static de.mi.common.Constants.*;

public class Main {
    public static void main(String[] args) {
        DBConnection.initDataBase();
        Server.start(HOST, PORT, NAMESPACE, LiteratureRest.APPLICATION);
        // start will block the programm
        // thus close will only be called when the server has been stoppen
        DBConnection.close();
    }
}
