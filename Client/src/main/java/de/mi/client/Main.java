package de.mi.client;

import de.mi.model.Connection;

import static de.mi.common.Constants.*;

public class Main {

    public static void main(String[] args) {
        var connection = new Connection(HOST, PORT, NAMESPACE, ClientType.MASTER);
        System.out.println(connection.getProgrammName());

        // TODO: start client
        connection.getBooks().orElseThrow().forEach(connection::removeBook);
    }
}
