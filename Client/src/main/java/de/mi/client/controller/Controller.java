package de.mi.client.controller;

import de.mi.client.ExceptionHandler;
import de.mi.common.Author;
import de.mi.common.Book;
import de.mi.common.ClientType;
import javafx.application.Platform;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.scene.control.Alert;
import javafx.scene.control.Button;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.TextArea;
import javafx.scene.control.TextField;

import java.util.Collections;
import java.util.NoSuchElementException;
import java.util.Optional;

public final class Controller extends ControllerBase {
    @FXML
    public ScrollPane subfieldsFilterPane;
    @FXML
    public TextField titleSearch;
    @FXML
    public TextField authorSearch;
    @FXML
    public TextField minYear;
    @FXML
    public TextField maxYear;
    @FXML
    public TextField minPages;
    @FXML
    public TextField maxPages;
    @FXML
    public ScrollPane previewPane;
    @FXML
    public TextArea selectionTitle;
    @FXML
    public ScrollPane authorSelectionPane;
    @FXML
    public TextField selectionPublisher;
    @FXML
    public TextField selectionYear;
    @FXML
    public TextField selectionPages;
    @FXML
    public ScrollPane subfieldSelectionPane;
    @FXML
    public Button updateButton;
    @FXML
    public Button createButton;

    @FXML
    public void applyFilter(ActionEvent actionEvent) {
        try {
            // TODO: load in background
            bookPreview.setBooks(connection.getBooks(filterProperties.getFilter()).orElseThrow(
                    () -> new NoSuchElementException("No books found")
            ));
        } catch (NoSuchElementException e) {
            ExceptionHandler.handle(e);
        }
    }

    @FXML
    public void clearFilter(ActionEvent actionEvent) {
        titleSearch.setText("");
        authorSearch.setText("");
        minYear.setText("");
        maxYear.setText("");
        minPages.setText("");
        maxPages.setText("");
        subfieldFilterSelector.setSelectedSubfields(subfields);
        applyFilter(actionEvent);
    }

    @FXML
    public void updateSelection(ActionEvent actionEvent) {
        // TODO: enable manipulation mode
        if (connection.getClientType() == ClientType.MASTER) {
            if (editable.get()) {
                getSelectedBook().ifPresent((Book book) -> {
                    Alert alert = new Alert(Alert.AlertType.INFORMATION);
                    alert.setContentText(String.format(
                            "%s%n%s%n%s",
                            book,
                            book.authors(),
                            book.subfields()
                    ));
                    alert.showAndWait();
                    connection.updateBook(book);
                });
            }
            editable.set(editable.not().get());
            updateButton.setText(editable.get() ? "Apply" : "Update");
        } else {
            ExceptionHandler.handle(new IllegalCallerException("insufficient privileges"));
        }
    }

    @FXML
    public void deleteSelection(ActionEvent actionEvent) {
        // TODO: delete book
        if (connection.getClientType() == ClientType.MASTER) {
            Alert alert = new Alert(Alert.AlertType.INFORMATION);
            Optional<Book> optionalBook = getSelectedBook();
            boolean result = optionalBook.map(connection::deleteBook).orElse(false);
            if (result) {
                alert.setHeaderText("Book removal was successful!");
                alert.setContentText("Deleted book: " + selectedBook);
            } else {
                String msg = optionalBook.map(Book::title)
                        .map("Subjective book: %s"::formatted)
                        .orElse("No book was selected");
                alert.setHeaderText("Book removal has failed");
                alert.setContentText(msg);
            }
            alert.show();
        } else {
            ExceptionHandler.handle(new IllegalCallerException("insufficient privileges"));
        }
    }

    @FXML
    public void addNew(ActionEvent actionEvent) {
        // TODO: enable manipulation mode and initialize new book creation
        if (connection.getClientType() == ClientType.MASTER) {
            if (editable.get()) {
                getSelectedBook().ifPresent((Book book) -> {
                    new Alert(Alert.AlertType.INFORMATION, book.toString()).showAndWait();
                    connection.createBook(book);
                });
            } else {
                var book = new Book(
                        connection.getNextID(Book.class),
                        null,
                        Collections.emptySet(),
                        null,
                        Book.DEFAULT_YEAR_RANGE.min(),
                        Book.DEFAULT_PAGE_RANGE.min(),
                        Collections.emptySet()
                );
                selectedBook.set(book);
            }
            editable.set(editable.not().get());
            createButton.setText(editable.get() ? "Apply" : "New");
        } else {
            ExceptionHandler.handle(new IllegalCallerException("insufficient privileges"));
        }
    }

    @FXML
    @Override
    public void initialize() {

        previewPane.setContent(bookPreview);

        filterProperties.titleSearchProperty().bindBidirectional(titleSearch.textProperty());
        filterProperties.authorSearchProperty().bindBidirectional(authorSearch.textProperty());
        filterProperties.minYearProperty().bindBidirectional(minYear.textProperty());
        filterProperties.maxYearProperty().bindBidirectional(maxYear.textProperty());
        filterProperties.minPagesProperty().bindBidirectional(minPages.textProperty());
        filterProperties.maxPagesProperty().bindBidirectional(maxPages.textProperty());
        filterProperties.subfieldsProperty().bindBidirectional(subfieldFilterSelector.subfieldsProperty());

        subfieldFilterSelector.setSelectedSubfields(subfields);
        subfieldFilterSelector.editableProperty().set(true);
        subfieldsFilterPane.setContent(subfieldFilterSelector);

        titleSearch.setOnAction(this::applyFilter);
        authorSearch.setOnAction(this::applyFilter);
        minYear.setOnAction(this::applyFilter);
        maxYear.setOnAction(this::applyFilter);
        minPages.setOnAction(this::applyFilter);
        maxPages.setOnAction(this::applyFilter);

        var authorPane = new AuthorPane(() -> connection.getNextID(Author.class));
        authorSelectionPane.setContent(authorPane);
        var subfieldPane = new SubfieldPane(subfields);
        subfieldSelectionPane.setContent(subfieldPane);

        selectedBook.titleProperty().bindBidirectional(selectionTitle.textProperty());
        selectedBook.authorsProperty().bindContentBidirectional(authorPane.authorProperties());
        selectedBook.publisherProperty().bindBidirectional(selectionPublisher.textProperty());
        selectedBook.yearProperty().bindBidirectional(selectionYear.textProperty());
        selectedBook.pagesProperty().bindBidirectional(selectionPages.textProperty());
        selectedBook.subfieldsProperty().bindContentBidirectional(subfieldPane.subfieldsProperty());

        editable.bindBidirectional(selectionTitle.editableProperty());
        editable.bindBidirectional(authorPane.editableProperty());
        editable.bindBidirectional(selectionPublisher.editableProperty());
        editable.bindBidirectional(selectionYear.editableProperty());
        editable.bindBidirectional(selectionPages.editableProperty());
        editable.bindBidirectional(subfieldPane.editableProperty());

        Platform.runLater(() -> {
            // TODO: load in background
            connection.getBooks().ifPresent(bookPreview::setBooks);
        });
    }

    private Optional<Book> getSelectedBook() {
        try {
            return Optional.of(selectedBook.get());
        } catch (RuntimeException e) {
            ExceptionHandler.handle(e);
            return Optional.empty();
        }
    }
}
