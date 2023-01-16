package de.mi.client.controller;

import de.mi.client.ExceptionHandler;
import de.mi.client.model.Connection;
import de.mi.client.model.ConnectionFactory;
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

import java.util.Collection;
import java.util.Collections;
import java.util.NoSuchElementException;
import java.util.Optional;
import java.util.Set;

abstract class ControllerBase {
    protected static final Book EMPTY_BOOK = new Book(
            0,
            null,
            Collections.emptySet(),
            null,
            Book.DEFAULT_YEAR_RANGE.min(),
            Book.DEFAULT_PAGE_RANGE.min(),
            Collections.emptySet()
    );
    private final Connection connection = ConnectionFactory.create();
    protected final Set<Subfield> subfields = connection.getSubfields().orElseThrow(
            () -> new NoSuchElementException("No subfields loaded")
    );
    protected final SubfieldPane subfieldFilterSelector = new SubfieldPane(subfields);
    protected final FilterProperties filterProperties = new FilterProperties();
    protected final BooleanProperty editable = new SimpleBooleanProperty(false);
    protected final BookProperties selectedBook = new BookProperties();
    protected final BookPreview bookPreview = new BookPreview((Book book) -> {
        if (editable.not().get()) selectedBook.set(book);
    });

    public abstract void initialize();

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
            if (isCreation) selectedBook.set(EMPTY_BOOK);
        }
    }

    public String getAppName() {
        return String.format("%s [%s]", connection.getProgrammName(), connection.getClientType().name());
    }

    protected final boolean havingSufficientPrivileges() {
        return connection.getClientType() == ClientType.MASTER;
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
                return new Task<>() {
                    @Override
                    protected Void call() throws RuntimeException {
                        try {
                            Collection<Book> books = (filter
                                    ? connection.getBooks(filterProperties.getFilter())
                                    : connection.getBooks()).orElseThrow();
                            Platform.runLater(() -> bookPreview.setBooks(books));
                        } catch (RuntimeException e) {
                            ExceptionHandler.handle(e);
                            throw e;
                        }
                        return null;
                    }
                };
            }
        };
        service.start();
    }
}
