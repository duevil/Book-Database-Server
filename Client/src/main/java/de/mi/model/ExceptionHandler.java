package de.mi.model;

import javax.swing.JOptionPane;

public final class ExceptionHandler {
    private ExceptionHandler() {
    }

    public static void handle(Exception e) {
        JOptionPane.showConfirmDialog(
                null,
                e,
                "Exception thrown!",
                JOptionPane.DEFAULT_OPTION,
                JOptionPane.ERROR_MESSAGE
        );
    }
}
