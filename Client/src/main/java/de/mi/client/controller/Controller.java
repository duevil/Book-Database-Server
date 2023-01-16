package de.mi.client.controller;

import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.scene.control.Alert;
import javafx.scene.control.Button;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.TextArea;
import javafx.scene.control.TextField;

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
    public Button deleteButton;
    @FXML
    public Button createButton;

    @FXML
    public void applyFilter(ActionEvent actionEvent) {
        loadBooks(true);
    }

    @FXML
    public void clearFilter(ActionEvent actionEvent) {
        titleSearch.setText("");
        authorSearch.setText("");
        minYear.setText("");
        maxYear.setText("");
        minPages.setText("");
        maxPages.setText("");
        subfieldFilterSelector.reset();
        applyFilter(actionEvent);
    }

    @FXML
    public void selectionEvent(ActionEvent event) {
        if (event.getSource() instanceof Button triggerButton
            && (triggerButton == updateButton ||
                triggerButton == deleteButton ||
                triggerButton == createButton)) {
            if (havingSufficientPrivileges()) {
                if (triggerButton == deleteButton) delete();
                else createOrUpdate(triggerButton, triggerButton == createButton);
            } else {
                Alert alert = new Alert(Alert.AlertType.WARNING);
                alert.setHeaderText("Unzureichende Berechtigung");
                alert.setContentText("Die nötige Berechtigung zum Ausführen der Aktion ist nicht vorhanden");
                alert.showAndWait();
            }
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

        subfieldFilterSelector.reset();
        subfieldFilterSelector.editableProperty().set(true);
        subfieldsFilterPane.setContent(subfieldFilterSelector);

        titleSearch.setOnAction(this::applyFilter);
        authorSearch.setOnAction(this::applyFilter);
        minYear.setOnAction(this::applyFilter);
        maxYear.setOnAction(this::applyFilter);
        minPages.setOnAction(this::applyFilter);
        maxPages.setOnAction(this::applyFilter);

        var authorPane = new AuthorPane();
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

        loadBooks(false);
    }
}
