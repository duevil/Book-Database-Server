package de.mi.client;

import javafx.application.Application;

/**
 * Hauptklasse des Programms; {@link Application#launch(Class, String...) launched} einen neuen {@link Client}
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
     * {@link Application#launch(Class, String...) Launched} einen neuen {@link Client}
     *
     * @param args nicht benutzt
     */
    public static void main(String[] args) {
        Application.launch(Client.class);
    }
}
