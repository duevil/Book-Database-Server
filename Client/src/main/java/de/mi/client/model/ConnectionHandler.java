package de.mi.client.model;

import de.mi.common.ClientType;
import jakarta.ws.rs.ProcessingException;
import jakarta.ws.rs.WebApplicationException;
import jakarta.ws.rs.core.Response;
import javafx.application.Platform;
import javafx.concurrent.Service;
import javafx.concurrent.Task;
import javafx.scene.control.Alert;
import javafx.scene.control.ChoiceDialog;

import java.net.ConnectException;
import java.util.Optional;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Klasse zum Ausführen und Abfangen bzw. Verarbeiten von eventuellen geworfenen Ausnahmen
 * der Methoden von {@link Connection}
 *
 * @author Malte Kasolowsky <code>m30114</code>
 */
final class ConnectionHandler {
    private static final Logger LOGGER = Logger.getLogger("org.glassfish");

    static {
        LOGGER.setLevel(Level.SEVERE);
    }

    private final Connection connection;

    /**
     * Konstruktor; öffnet einen Dialog zur Auswahl der {@link ClientType Client-Art}
     * und öffnet eine neue {@link Connection};
     * wurde keine Client-Art ausgewählt, so wird die Anwendung {@link System#exit(int) geschlossen}
     */
    ConnectionHandler() {
        var dialog = new ChoiceDialog<>(null, ClientType.values());
        dialog.setHeaderText("Choose Client Type: ");
        this.connection = dialog.showAndWait().map(Connection::new).orElseGet(() -> {
            var alert = new Alert(Alert.AlertType.INFORMATION);
            alert.setHeaderText("No Client Type chosen");
            alert.setContentText("Program will be closed...");
            alert.showAndWait();
            System.exit(0);
            return null;
        });
    }

    /**
     * Führt eine {@link Connection}-Methode, welche ein Ergebnis zurückgibt,
     * mittels einer {@link Service} aus
     *
     * @param connectionFunction Die Methode der Verbindung, welche ausgeführt werden soll
     * @param resultConsumer     Ein Consumer, welcher das Ergebnis der Anfrage in einem JavaFX-Thread übergeben bekommt
     * @param onFailAction       Ein {@link Runnable}, welches beim Auftreten eines Fehlers ausgeführt wird
     * @param <T>                Der Typ der Rückgabe der Anfrage
     */
    public <T> void handle(Function<Connection, T> connectionFunction,
                           Consumer<T> resultConsumer,
                           Runnable onFailAction) {
        new Service<Void>() {
            @Override
            protected Task<Void> createTask() {
                return new Task<>() {
                    @Override
                    protected Void call() {
                        handle0(null, (c, param) -> connectionFunction.apply(c), resultConsumer, onFailAction);
                        return null;
                    }
                };
            }
        }.start();
    }

    /**
     * Führt eine {@link Connection}-Methode, welche ein Argument übergeben bekommt und Ergebnis zurückgibt,
     * mittels einer {@link Service} aus
     *
     * @param functionArgument   Ein Argument, welches der Verbindungsmethode übergeben wird
     * @param connectionFunction Die Methode der Verbindung, welche ausgeführt werden soll
     * @param resultConsumer     Ein Consumer, welcher das Ergebnis der Anfrage in einem JavaFX-Thread übergeben bekommt
     * @param onFailAction       Ein {@link Runnable}, welches beim Auftreten eines Fehlers ausgeführt wird
     * @param <R>                Der Typ des Arguments für die Anfrage
     * @param <T>                Der Typ der Rückgabe der Anfrage
     */
    public <R, T> void handle(R functionArgument,
                              BiFunction<Connection, R, T> connectionFunction,
                              Consumer<T> resultConsumer,
                              Runnable onFailAction) {
        new Service<Void>() {
            @Override
            protected Task<Void> createTask() {
                return new Task<>() {
                    @Override
                    protected Void call() {
                        handle0(functionArgument, connectionFunction, resultConsumer, onFailAction);
                        return null;
                    }
                };
            }
        }.start();
    }

    /**
     * Führt eine {@link Connection}-Methode, welche ein Argument übergeben bekommt, aus
     *
     * @param functionArgument   Ein Argument, welches der Verbindungsmethode übergeben wird
     * @param connectionFunction Die Methode der Verbindung, welche ausgeführt werden soll
     * @param onFailAction       Ein {@link Runnable}, welches beim Auftreten eines Fehlers ausgeführt wird
     * @param <R>                Der Typ des Arguments für die Anfrage
     * @apiNote Diese Methode wird <b>nicht</b> über eine {@link Service} ausgeführt,
     * da es aus unbekannten Gründen zu diversen Fehlern beim Aufruf von JDBC-Methoden auf dem Server kommt
     */
    public <R> void handle(R functionArgument,
                           BiConsumer<Connection, R> connectionFunction,
                           Runnable onFailAction) {
        handle0(functionArgument, (Connection c, R param) -> {
            connectionFunction.accept(c, param);
            return null;
        }, null, onFailAction);
    }

    /**
     * Erwartet eine {@link BiFunction}, welche eine Methode von {@link Connection} darstellt
     * und über die übergebene Connection ausgeführt werden soll.
     * Ist ein {@link Consumer} übergeben worden,
     * so wird das Ergebnis der Funktion ein einem {@link Platform#runLater(Runnable) JavaFX-Thread}
     * an den Consumer übergeben.
     * Wird bei der Ausführung einer Anfrage an den Server eine Ausnahme geworfen,
     * so wird diese abgefangen und die Information über den Fehler
     * in einem entsprechenden {@link Alert Fenster} dem Nutzer präsentiert
     * und die übergebene Fehler-Aktion ausgeführt
     *
     * @param functionArgument   Ein Argument, welches der Verbindungsmethode übergeben wird
     * @param connectionFunction Die Methode der Verbindung, welche ausgeführt werden soll
     * @param resultConsumer     Ein Consumer, welcher das Ergebnis der Anfrage in einem JavaFX-Thread übergeben bekommt
     * @param onFailAction       Ein {@link Runnable}, welches beim Auftreten eines Fehlers ausgeführt wird
     * @param <R>                Der Typ des Arguments für die Anfrage
     * @param <T>                Der Typ der Rückgabe der Anfrage
     */
    private <R, T> void handle0(R functionArgument, BiFunction<Connection, R, T> connectionFunction,
                                Consumer<T> resultConsumer, Runnable onFailAction) {
        Exception ex;
        try {
            final T result = connectionFunction.apply(connection, functionArgument);
            if (resultConsumer != null) Platform.runLater(() -> resultConsumer.accept(result));
            return;

        } catch (ProcessingException e) {
            ex = e;
            if (e.getCause() instanceof ConnectException) {
                Platform.runLater(() -> {
                    var alert = new Alert(Alert.AlertType.ERROR);
                    alert.setHeaderText("Server Connection Error");
                    alert.setContentText("Unable to connect to the server\n\nProgram will be closed...");
                    alert.showAndWait();
                    Platform.exit();
                });
            }
        } catch (WebApplicationException e) {
            ex = e;
            Platform.runLater(() -> {
                var alert = new Alert(switch (e.getResponse().getStatusInfo().getFamily()) {
                    case SERVER_ERROR -> Alert.AlertType.ERROR;
                    case CLIENT_ERROR -> Alert.AlertType.WARNING;
                    default -> Alert.AlertType.INFORMATION;
                });
                alert.setHeaderText("Error: " + e.getMessage());
                Optional.ofNullable(e.getCause()).map(Throwable::getMessage).ifPresent(alert::setContentText);
                if (e.getResponse().getStatusInfo().getFamily() == Response.Status.Family.SERVER_ERROR) {
                    String text = alert.getContentText();
                    alert.setContentText(text + "\n\nPlease contact customer support");
                }
                alert.showAndWait();
            });
        } catch (RuntimeException e) {
            ex = e;
        }
        Optional.ofNullable(onFailAction).ifPresent(Platform::runLater);
        LOGGER.log(Level.WARNING, "Connection request failed", ex);
    }
}
