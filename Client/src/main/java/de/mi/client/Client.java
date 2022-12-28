package de.mi.client;

import de.mi.model.ExceptionHandler;
import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Scene;
import javafx.stage.Stage;

import java.io.IOException;
import java.util.NoSuchElementException;
import java.util.Optional;

public class Client extends Application {
    /**
     * TODO: translate
     * The main entry point for all JavaFX applications.
     * The start method is called after the init method has returned,
     * and after the system is ready for the application to begin running.
     *
     * <p>
     * NOTE: This method is called on the JavaFX Application Thread.
     * </p>
     *
     * @param primaryStage the primary stage for this application, onto which
     *                     the application scene can be set.
     *                     Applications may create other stages, if needed, but they will not be
     *                     primary stages.
     */
    @Override
    public void start(final Stage primaryStage) {
        var value = ClassLoader.getSystemResource("view.fxml");
        try {
            FXMLLoader loader = Optional.ofNullable(value).map(FXMLLoader::new).orElseThrow(
                    () -> new NoSuchElementException("View FXML resource not found")
            );
            var scene = new Scene(loader.load());
            primaryStage.setScene(scene);
            var con = ((Controller) loader.getController()).getConnection();
            primaryStage.setTitle(con.getProgrammName() + " [" + con.getClientType().name() + ']');
            primaryStage.show();
        } catch (NoSuchElementException | IOException e) {
            ExceptionHandler.handle(e);
        }
    }
}
