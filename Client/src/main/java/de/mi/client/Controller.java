package de.mi.client;

import de.mi.common.Author;
import de.mi.common.Book;
import de.mi.common.BookFilter;
import de.mi.common.ClientType;
import de.mi.common.Subfield;
import de.mi.model.Connection;
import de.mi.model.ConnectionFactory;
import de.mi.model.ExceptionHandler;
import javafx.application.Platform;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.scene.Cursor;
import javafx.scene.Node;
import javafx.scene.control.CheckBox;
import javafx.scene.control.Label;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.TextArea;
import javafx.scene.control.TextField;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;

import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Optional;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collectors;

public final class Controller {
    public static final double SPACING = 10D;
    @FXML
    public ScrollPane filterSubfields;
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
    public ScrollPane selectionAuthors;
    @FXML
    public TextField selectionPublisher;
    @FXML
    public TextField selectionYear;
    @FXML
    public TextField selectionPages;
    @FXML
    public ScrollPane selectionSubfields;
    private final Connection connection = ConnectionFactory.create();
    private final Map<Subfield, Boolean> subfields = connection.getSubfields()
            .orElseThrow()
            .stream()
            .collect(Collectors.toMap(Function.identity(), s -> true));

    private static <T> Optional<T> readTextField(TextField field, Class<T> tClass) {
        String text = field.getText();
        return Optional.ofNullable(text)
                .filter(s -> !s.isBlank())
                .map((String s) -> {
                    if (String.class.equals(tClass)) return s;
                    if (Integer.class.equals(tClass)) try {
                        return Integer.parseInt(s);
                    } catch (NumberFormatException e) {
                        ExceptionHandler.handle(e);
                    }
                    return null;
                })
                .map(tClass::cast);
    }

    private static <T> void setScrollPaneContent(
            ScrollPane pane,
            Supplier<List<T>> contentSupplier,
            Function<T, Node> nodeFunction,
            double spacing) {
        var nodes = contentSupplier.get().stream().map(nodeFunction).toArray(Node[]::new);
        pane.setContent(new VBox(spacing, nodes));
    }

    @FXML
    public void applyFilter(ActionEvent actionEvent) {
        try {
            // TODO: load in background
            setScrollPaneContent(previewPane,
                    connection.getBooks(createFilter())::orElseThrow,
                    this::bookToNode,
                    SPACING);
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
        ((VBox) filterSubfields.getContent())
                .getChildren()
                .forEach(node -> ((CheckBox) node).setSelected(true));
        subfields.keySet().forEach(subfield -> subfields.put(subfield, true));
        applyFilter(actionEvent);
    }

    @FXML
    public void updateSelection(ActionEvent actionEvent) {
        // TODO: enable manipulation mode
        if (connection.getClientType() == ClientType.MASTER) {
            ExceptionHandler.handle(new UnsupportedOperationException("not yet implemented"));
        } else {
            ExceptionHandler.handle(new IllegalCallerException("insufficient privileges"));
        }
    }

    @FXML
    public void deleteSelection(ActionEvent actionEvent) {
        // TODO: delete book
        if (connection.getClientType() == ClientType.MASTER) {
            ExceptionHandler.handle(new UnsupportedOperationException("not yet implemented"));
        } else {
            ExceptionHandler.handle(new IllegalCallerException("insufficient privileges"));
        }
    }

    @FXML
    public void addNew(ActionEvent actionEvent) {
        // TODO: enable manipulation mode and initialize new book creation
        if (connection.getClientType() == ClientType.MASTER) {
            ExceptionHandler.handle(new UnsupportedOperationException("not yet implemented"));
        } else {
            ExceptionHandler.handle(new IllegalCallerException("insufficient privileges"));
        }
    }

    @FXML
    public void initialize() {
        Platform.runLater(() -> {
            try {
                var subfieldPane = new VBox(SPACING);
                subfields.forEach((Subfield s, Boolean value) -> {
                    var checkBox = new CheckBox(s.name());
                    checkBox.setSelected(value);
                    checkBox.setOnAction(event -> subfields.put(s, checkBox.isSelected()));
                    subfieldPane.getChildren().add(checkBox);
                });
                filterSubfields.setContent(subfieldPane);
                // TODO: load in background
                setScrollPaneContent(previewPane,
                        connection.getBooks()::orElseThrow,
                        this::bookToNode,
                        SPACING);
            } catch (NoSuchElementException e) {
                ExceptionHandler.handle(e);
            }
        });
    }

    private Node bookToNode(final Book book) {
        var node = new Label(book.toString());
        node.setOnMousePressed((MouseEvent event) -> {
            if (event.isPrimaryButtonDown()) setSelection(book);
        });
        node.setCursor(Cursor.HAND);
        node.setWrapText(true);
        return node;
    }

    private BookFilter createFilter() {
        var builder = BookFilter.builder();
        readTextField(titleSearch, String.class).ifPresent(builder::searchTitle);
        readTextField(authorSearch, String.class).ifPresent(builder::searchAuthor);
        readTextField(minYear, Integer.class).ifPresent(builder::minYear);
        readTextField(maxYear, Integer.class).ifPresent(builder::maxYear);
        readTextField(minPages, Integer.class).ifPresent(builder::minPages);
        readTextField(maxPages, Integer.class).ifPresent(builder::maxPages);
        subfields.entrySet()
                .stream()
                .filter(Map.Entry::getValue)
                .map(Map.Entry::getKey)
                .map(Subfield::id)
                .forEach(builder::subfield);
        return builder.build();
    }

    private void setSelection(Book book) {
        selectionTitle.setText(book.title());
        selectionPublisher.setText(book.publisher());
        selectionYear.setText(String.valueOf(book.year()));
        selectionPages.setText(String.valueOf(book.pages()));
        setScrollPaneContent(selectionAuthors,
                book.authors().stream().map((Author a) -> {
                    var firstName = new TextField(a.firstName());
                    var lastName = new TextField(a.lastName());
                    // TODO: toggle when manipulating
                    firstName.setEditable(false);
                    lastName.setEditable(false);
                    firstName.setPromptText("author's first name");
                    lastName.setPromptText("author's last name");
                    return new Node[]{firstName, lastName};
                })::toList,
                HBox::new,
                0D);
        setScrollPaneContent(selectionSubfields,
                book.subfields().stream().map((Subfield s) -> {
                    // TODO: choice box when manipulating
                    var subfield = new TextField(s.name());
                    subfield.setEditable(false); // TODO: toggle when manipulating
                    return subfield;
                })::toList,
                Node.class::cast,
                0D);
    }

    Connection getConnection() {
        return connection;
    }
}
