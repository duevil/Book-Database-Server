package de.mi.client;

import javafx.scene.control.Alert;

/**
 * Utility-Klasse zum Verarbeiten von Ausnahmen
 *
 * @author Malte Kasolowsky <code>m30114</code>
 */
public final class ExceptionHandler {
    /**
     * Privater Konstruktor; eine Erzeugung einer Klassen-Instanz ist nicht nötig
     */
    private ExceptionHandler() {
    }

    /**
     * Verarbeitet eine Ausnahme; diese wird in einem JavaFX-{@link Alert}
     * zusammen mit ihren, falls vorhanden, auslösenden Ausnahmen angezeigt
     *
     * @param e Die zu verarbeitende Ausnahme
     */
    public static void handle(Throwable e) {
        var alert = new Alert(Alert.AlertType.ERROR);
        alert.setHeaderText("Eine Ausnahme ist aufgetreten!");
        alert.setContentText(fromThrowable(e));
        alert.showAndWait();
    }

    /**
     * Wandelt eine Ausnahme in einen String bestehend aus Name, Nachricht und optional auslösender Ausnahme
     *
     * @param e Die zu verarbeitende Ausnahme
     * @return Die erzeugte Zeichenkette mit Informationen über die verarbeitete Ausnahme
     */
    private static String fromThrowable(Throwable e) {
        StringBuilder sb = new StringBuilder(e.getClass().getSimpleName());
        sb.append(": ");
        sb.append(e.getMessage());
        return fromThrowable(e.getCause(), sb);
    }

    /**
     * Wandelt eine Ausnahme in eine Zeichenkette mit ihren Informationen um
     * und führt dies rekursiv für mögliche Auslöser aus
     *
     * @param e  Die zu verarbeitende Ausnahme
     * @param sb Ein {@link StringBuilder}, mit welchen die Zeichenkette gebaut wird
     * @return Eine Zeichenkette mit den Informationen der verarbeiteten Ausnahme
     * oder den String-Wert des StringBuilders, falls die Ausnahme null ist
     */
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
