package de.mi.client.model;

import jakarta.ws.rs.ProcessingException;
import jakarta.ws.rs.WebApplicationException;
import javafx.application.Platform;
import javafx.scene.control.Alert;

import java.net.ConnectException;
import java.util.Optional;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.logging.Level;
import java.util.logging.Logger;

final class ConnectionHandler {
    private static final Logger LOGGER = Logger.getAnonymousLogger();

    static {
        LOGGER.setLevel(Level.SEVERE);
    }

    private final Connection connection;

    ConnectionHandler(Supplier<Connection> connectionSupplier) {
        this.connection = connectionSupplier.get();
    }

    private static <R, T> void run(Connection connection,
                                   R functionParameter,
                                   BiFunction<Connection, R, T> connectionFunction,
                                   Consumer<T> resultConsumer,
                                   Runnable onFailAction) {
        Exception ex;
        try {
            final T result = connectionFunction.apply(connection, functionParameter);
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
                alert.showAndWait();
            });
        } catch (RuntimeException e) {
            ex = e;
        }
        Optional.ofNullable(onFailAction).ifPresent(Platform::runLater);
        LOGGER.log(Level.WARNING, "Connection request failed", ex);
    }

    public <R, T> void handle(R functionParameter,
                              BiFunction<Connection, R, T> connectionFunction,
                              Consumer<T> resultConsumer,
                              Runnable onFailAction) {
        run(connection, functionParameter, connectionFunction, resultConsumer, onFailAction);
    }

    public <T> void handle(Function<Connection, T> connectionFunction,
                           Consumer<T> resultConsumer,
                           Runnable onFailAction) {
        handle(null, (c, param) -> connectionFunction.apply(c), resultConsumer, onFailAction);
    }

    public <R> void handle(R parameter,
                           BiConsumer<Connection, R> connectionFunction,
                           Runnable onFailAction) {
        handle(parameter, (Connection c, R param) -> {
            connectionFunction.accept(c, param);
            return null;
        }, null, onFailAction);
    }
}
