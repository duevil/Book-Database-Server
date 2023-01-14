package de.mi.client;

import javafx.scene.control.Alert;

public final class ExceptionHandler {
    private ExceptionHandler() {
    }

    public static void handle(Throwable e) {
        var alert = new Alert(Alert.AlertType.ERROR);
        alert.setHeaderText("Eine Ausnahme ist aufgetreten!");
        alert.setContentText(fromThrowable(e));
        alert.show();
    }

    private static String fromThrowable(Throwable e) {
        StringBuilder sb = new StringBuilder(e.getClass().getSimpleName());
        sb.append(": ");
        sb.append(e.getMessage());
        return fromThrowable(e.getCause(), sb);
    }

    private static String fromThrowable(Throwable e, StringBuilder sb) {
        if (e != null) {
            sb.append(System.lineSeparator());
            sb.append("Grund: ");
            sb.append(e.getClass().getSimpleName());
            sb.append(": ");
            sb.append(e.getMessage());
            return fromThrowable(e.getCause(), sb);
        }
        return sb.toString();
    }
}
