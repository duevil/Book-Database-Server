package de.mi.client;

import de.mi.model.Connection;
import javafx.fxml.FXML;

public abstract class Controller {
    private Connection connection;

    @FXML
    protected abstract void initialize();

    protected abstract void onConnectionUpdate();

    public final Connection getConnection() {
        return connection;
    }

    public final void setConnection(Connection connection) {
        this.connection = connection;
        onConnectionUpdate();
    }
}
