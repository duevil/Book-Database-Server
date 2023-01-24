package de.mi.client;

import javafx.application.Application;

/**
 * Hauptklasse des Programms; {@link Application#launch(Class, String...) launched} einen neuen {@link Client}
 *
 * @author Malte Kasolowsky <code>m30114</code>
 */
public class Main {
    /**
     * {@link Application#launch(Class, String...) Launched} einen neuen {@link Client}
     *
     * @param args nicht benutzt
     */
    public static void main(String[] args) {
        Application.launch(Client.class);
    }
}
