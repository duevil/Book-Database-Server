package de.mi.client.model;

import de.mi.common.Book;
import de.mi.common.BookFilter;
import de.mi.common.ClientType;
import de.mi.common.Subfield;
import javafx.beans.property.SimpleListProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.collections.FXCollections;
import javafx.scene.control.Alert;
import javafx.scene.control.ChoiceDialog;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class Model {
    private final SimpleObjectProperty<String> programmName = new SimpleObjectProperty<>();
    private final SimpleListProperty<Book> loadedBooks = new SimpleListProperty<>(FXCollections.observableArrayList());
    private final ConnectionHandler handler = new ConnectionHandler(Model::createConnection);
    private final Set<Subfield> subfields = new HashSet<>();
    private Runnable onFailAction;

    public Model() {
        handler.handle(Connection::getProgrammName, programmName::set, null);
        handler.handle(Connection::getSubfields, subfields::addAll, null);
    }

    private static Connection createConnection() {
        var dialog = new ChoiceDialog<>(null, ClientType.values());
        dialog.setHeaderText("Choose Client Type: ");
        return dialog.showAndWait().map(Connection::new).orElseGet(() -> {
            var alert = new Alert(Alert.AlertType.INFORMATION);
            alert.setHeaderText("No Client Type chosen");
            alert.setContentText("Program will be closed...");
            alert.showAndWait();
            System.exit(0);
            return null;
        });
    }

    public Set<Subfield> getSubfields() {
        return Collections.unmodifiableSet(subfields);
    }

    public SimpleObjectProperty<String> getProgrammName() {
        return programmName;
    }

    public SimpleListProperty<Book> getLoadedBooks() {
        return loadedBooks;
    }

    public void loadBooks(BookFilter filter) {
        handler.handle(filter, Connection::getBooks, this::setBooks, onFailAction);
    }

    public void loadBooks() {
        handler.handle(Connection::getBooks, this::setBooks, onFailAction);
    }

    public void updateBook(Book book) {
        handler.handle(book, Connection::updateBook, onFailAction);
    }

    public void deleteBook(Book book) {
        handler.handle(book, Connection::deleteBook, onFailAction);
    }

    public void createBook(Book book) {
        handler.handle(book, Connection::createBook, onFailAction);
    }

    public void setOnFailAction(Runnable onFailAction) {
        this.onFailAction = onFailAction;
    }

    private void setBooks(List<Book> books) {
        loadedBooks.clear();
        loadedBooks.setAll(books);
    }
}
