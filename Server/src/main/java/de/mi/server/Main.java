package de.mi.server;

public class Main {
    private static final String HOST = "0.0.0.0";
    private static final int PORT = 8080;
    private static final String SERVER_NAME = "informatik";
    public static void main(String[] args) {
        // TODO: start server
        Server.start(HOST, PORT, SERVER_NAME, LiteratureRest.getApplicationClass());
    }
}
