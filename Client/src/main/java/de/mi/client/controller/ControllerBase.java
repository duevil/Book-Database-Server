package de.mi.client.controller;

import de.mi.client.ExceptionHandler;
import de.mi.client.model.Connection;
import de.mi.common.Book;
import de.mi.common.ClientType;
import de.mi.common.Subfield;
import javafx.application.Platform;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.concurrent.Service;
import javafx.concurrent.Task;
import javafx.scene.control.Alert;
import javafx.scene.control.Button;
import javafx.scene.control.ButtonType;
import javafx.scene.control.ChoiceDialog;

import java.util.Collection;
import java.util.NoSuchElementException;
import java.util.Optional;
import java.util.Set;

/* TODO
    -> filter apply prevention on editable
    -> editable type for invalid apply prevention
    -> proper alert for invalid input with option for clearing
 */

abstract class ControllerBase {
    protected final BooleanProperty editable = new SimpleBooleanProperty(this, "editable", false);
    protected final FilterProperties filterProperties = new FilterProperties();
    protected final BookProperties selectedBook = new BookProperties();
    protected final BookPreview bookPreview = new BookPreview((Book book) -> {
        if (editable.not().get()) selectedBook.set(book);
    });
    private final Connection connection = createConnection();
    protected final Set<Subfield> subfields = connection.getSubfields().orElseThrow(
            () -> new NoSuchElementException("No subfields loaded")
    );
    protected final SubfieldPane subfieldFilterSelector = new SubfieldPane(subfields);

    private static Connection createConnection() {
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
                });
    }

    public abstract void initialize();

    protected final void selectionAction(Button triggerButton, boolean isUpdate, boolean isDelete, boolean isCreate) {
        if (!connection.getClientType().isMaster()) {
            throw new IllegalCallerException("client is not master");
        } else if (isUpdate) {
            // TODO
            createOrUpdate(triggerButton, false);
        } else if (isDelete) {
            // TODO
            delete();
        } else if (isCreate) {
            // TODO
            createOrUpdate(triggerButton, true);
        } else {
            throw new IllegalArgumentException("action is neither of update, delete or create");
        }
    }

    protected final void delete() {
        Alert confirm = new Alert(Alert.AlertType.CONFIRMATION);
        confirm.setHeaderText("Löschen bestätigen");
        confirm.setContentText("Soll das Buch '" +
                               selectedBook.titleProperty().get() +
                               "' wirklich gelöscht werden?");
        var result = confirm.showAndWait();
        if (result.isPresent() && result.get() == ButtonType.OK) {
            boolean success = getSelectedBook().map(connection::deleteBook).orElse(false);
            Alert alert = new Alert(success
                    ? Alert.AlertType.INFORMATION
                    : Alert.AlertType.WARNING);
            alert.setHeaderText(success
                    ? "Das Löschen war erfolgreich"
                    : "Buch konnte nicht gelöscht werden");
            alert.showAndWait();
            if (success) loadBooks(false);
        }
    }

    protected final void createOrUpdate(Button triggerButton, boolean isCreation) {
        boolean isEditable = editable.get();
        if (isEditable) {
            boolean success = (isCreation
                    ? getSelectedBook().map(connection::createBook)
                    : getSelectedBook().map(connection::updateBook)).orElse(false);
            if (success) {
                editable.set(false);
                triggerButton.setText(isCreation ? "New" : "Update");
                loadBooks(false);
            } else {
                Alert alert = new Alert(Alert.AlertType.WARNING);
                alert.setHeaderText("Aktion konnte nicht durchgeführt werden");
                alert.showAndWait();
            }
        } else {
            editable.set(true);
            triggerButton.setText("Apply");
            if (isCreation) selectedBook.clear();
        }
    }

    public String getAppName() {
        return String.format("%s [%s]", connection.getProgrammName(), connection.getClientType().name());
    }

    protected final Optional<Book> getSelectedBook() {
        try {
            return Optional.of(selectedBook.get());
        } catch (RuntimeException e) {
            ExceptionHandler.handle(e);
            return Optional.empty();
        }
    }

    protected final void loadBooks(boolean filter) {
        Service<Void> service = new Service<>() {
            @Override
            protected Task<Void> createTask() {
                return new BookLoadingTask(filter);
            }
        };
        service.start();
    }

    private class BookLoadingTask extends Task<Void> {
        private final boolean filter;

        public BookLoadingTask(boolean filter) {
            this.filter = filter;
        }

        @Override
        protected Void call() throws RuntimeException {
            try {
                Collection<Book> books = (filter
                        ? connection.getBooks(filterProperties.get())
                        : connection.getBooks()).orElseThrow();
                Platform.runLater(() -> bookPreview.setBooks(books));
                if (filter) Platform.runLater(ControllerBase.this.selectedBook::clear);
                else Platform.runLater(filterProperties::clear);
            } catch (RuntimeException e) {
                Platform.runLater(() -> ExceptionHandler.handle(e));
                throw e;
            }
            return null;
        }
    }
}
