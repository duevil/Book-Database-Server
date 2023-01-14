package de.mi.client.model;

import de.mi.common.ClientType;
import javafx.scene.control.Alert;
import javafx.scene.control.ChoiceDialog;

public final class ConnectionFactory {
    private ConnectionFactory() {
    }

    public static Connection create() {
        var dialog = new ChoiceDialog<>(null, ClientType.values());
        dialog.setHeaderText("Wähle die Art des Clients");
        return dialog.showAndWait()
                .map(Connection::new)
                .orElseGet(() -> {
                            var alert = new Alert(Alert.AlertType.INFORMATION);
                            alert.setHeaderText("Kein Client Typ ausgewählt!");
                            alert.setContentText("Das Programm wird geschlossen ...");
                            alert.showAndWait();
                            System.exit(0);
                            return null;
                        }
                );
    }
}
