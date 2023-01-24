package de.mi.client;

import de.mi.client.controller.Controller;
import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Scene;
import javafx.stage.Stage;

import java.io.IOException;
import java.util.Locale;
import java.util.NoSuchElementException;
import java.util.Optional;

/**
 * Ein grafischer Client zum Präsentieren, Abfragen und möglichen Manipulieren von Buchdaten
 *
 * @author Malte Kasolowsky <code>m30114</code>
 */
public class Client extends Application {
    /**
     * Lädt die View der Oberfläche mittels eines {@link FXMLLoader} aus den Ressourcen
     * und baut daraus die Oberfläche
     *
     * @param primaryStage Die primäre {@link Stage} der Application,
     *                     auf der die Oberfläche aufgebaut wird
     */
    @Override
    public void start(final Stage primaryStage) {
        Locale.setDefault(Locale.ENGLISH);
        var value = ClassLoader.getSystemResource("view.fxml");
        try {
            FXMLLoader loader = Optional.ofNullable(value).map(FXMLLoader::new).orElseThrow(
                    () -> new NoSuchElementException("View FXML resource not found")
            );
            var scene = new Scene(loader.load());
            primaryStage.setScene(scene);
            primaryStage.titleProperty().bind(loader.<Controller>getController().getAppName());
            primaryStage.show();
        } catch (NoSuchElementException | IOException e) {
            ExceptionHandler.handle(e);
        }
    }
}
