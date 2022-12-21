package de.mi.client;

import de.mi.common.ClientType;
import de.mi.model.Connection;
import de.mi.model.ExceptionHandler;
import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.fxml.LoadException;
import javafx.scene.Scene;
import javafx.scene.control.Alert;
import javafx.scene.control.ChoiceDialog;
import javafx.stage.Stage;

import java.io.IOException;
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
        var dialog = new ChoiceDialog<>(null, ClientType.values());
        dialog.setHeaderText("Wähle die Art des Clients");
        dialog.showAndWait().ifPresentOrElse(
                (ClientType clientType) -> {
                    try {
                        final Connection connection = new Connection(clientType);
                        var value = ClassLoader.getSystemResource("main.fxml");
                        final FXMLLoader loader = Optional.ofNullable(value)
                                .map(FXMLLoader::new)
                                .orElseThrow(LoadException::new);
                        primaryStage.setTitle(connection.getProgrammName() + " [" + clientType.name() + ']');
                        primaryStage.setScene(new Scene(loader.load()));
                        primaryStage.show();
                        ((Controller) loader.getController()).setConnection(connection);
                    } catch (IOException e) {
                        ExceptionHandler.handle(e);
                    }
                },
                () -> {
                    var alert = new Alert(Alert.AlertType.INFORMATION);
                    alert.setHeaderText("Kein Client Typ ausgewählt!");
                    alert.setContentText("Das Programm wird geschlossen ...");
                    alert.show();
                }
        );
    }
}
