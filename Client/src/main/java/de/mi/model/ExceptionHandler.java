package de.mi.model;

import javafx.scene.control.Alert;

public final class ExceptionHandler {
    private ExceptionHandler() {
    }

    public static void handle(Exception e) {
        var alert = new Alert(Alert.AlertType.ERROR);
        alert.setHeaderText("Eine Ausnahme wurde geworfen!");
        alert.setContentText(e.toString());
        alert.show();
    }
}
