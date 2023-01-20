package de.mi.client.controller;

import de.mi.common.Book;
import javafx.application.Platform;
import javafx.beans.binding.When;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.geometry.Insets;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.Slider;
import javafx.scene.control.TextArea;
import javafx.scene.control.TextField;
import javafx.scene.layout.Pane;
import javafx.scene.layout.VBox;

@SuppressWarnings({"java:S109", "java:S1820", "java:S3242", "java:S2211", "java:S1135"}) // TODO: remove suppression
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
    public Slider minRatingSlider;
    @FXML
    public Label minRating;
    @FXML
    public Slider maxRatingSlider;
    @FXML
    public Label maxRating;
    @FXML
    public TextArea selectionTitle;
    @FXML
    public ScrollPane selectionAuthorPane;
    @FXML
    public TextField selectionPublisher;
    @FXML
    public TextField selectionYear;
    @FXML
    public TextField selectionPages;
    @FXML
    public Pane selectionRatingPane;
    @FXML
    public ScrollPane selectionSubfieldsPane;
    @FXML
    public Button updateButton;
    @FXML
    public Button deleteButton;
    @FXML
    public Button createButton;
    @FXML
    public ListView<Book> bookView;
    @FXML
    public VBox filterMainPane;
    @FXML
    public VBox previewMainPane;
    @FXML
    public VBox selectionMainPain;

    @FXML
    public void applyFilter() {
        loadBooks(true);
    }

    @FXML
    public void clearFilter() {
        selectedBookProperties.clear();
        filterProperties.clear();
        applyFilter();
    }

    @FXML
    public void selectionEvent(ActionEvent event) {
        if (event.getSource() instanceof Button triggerButton)
            selectionAction(
                    triggerButton == updateButton,
                    triggerButton == deleteButton,
                    triggerButton == createButton);
    }

    @FXML
    @Override
    public void initialize() {
        initialiseMainPanes();
        initialiseSelection();
        initialiseFilter();
        initialiseButtons();
        initializeBookView();
        loadBooks(false);
    }

    private void initialiseMainPanes() {
        filterMainPane.prefWidthProperty().bind(previewMainPane.widthProperty());
        selectionMainPain.prefWidthProperty().bind(previewMainPane.widthProperty());
        previewMainPane.prefWidthProperty().bind(previewMainPane.widthProperty().multiply(1.5));
    }

    private void initialiseSelection() {
        var selectionAuthors = new AuthorPane();
        Util.bindScrollPaneContentOrLabel(
                selectionAuthorPane,
                selectionAuthors,
                "Selected book's authors",
                selectionAuthors.authorProperties().emptyProperty().not().or(isSelectionEditable));

        var selectionSubfields = new SubfieldPane(model.getSubfields());
        Util.bindScrollPaneContentOrLabel(
                selectionSubfieldsPane,
                selectionSubfields,
                "Selected book's subfields",
                selectionSubfields.subfieldsProperty().emptyProperty().not().or(isSelectionEditable));

        var selectionRating = new RatingPane();
        selectionRatingPane.getChildren().add(selectionRating);

        Util.bindProperties(selectedBookProperties.titleProperty(), selectionTitle.textProperty());
        Util.bindProperties(selectedBookProperties.authorsProperty(), selectionAuthors.authorProperties());
        Util.bindProperties(selectedBookProperties.publisherProperty(), selectionPublisher.textProperty());
        Util.bindProperties(selectedBookProperties.yearProperty(), selectionYear.textProperty());
        Util.bindProperties(selectedBookProperties.pagesProperty(), selectionPages.textProperty());
        Util.bindProperties(selectedBookProperties.ratingProperty(), selectionRating.ratingProperty());
        Util.bindProperties(selectedBookProperties.subfieldsProperty(), selectionSubfields.subfieldsProperty());

        Util.addFormatter(selectionYear);
        Util.addFormatter(selectionPages);

        selectionTitle.editableProperty().bind(isSelectionEditable);
        selectionAuthors.editableProperty().bind(isSelectionEditable);
        selectionPublisher.editableProperty().bind(isSelectionEditable);
        selectionYear.editableProperty().bind(isSelectionEditable);
        selectionPages.editableProperty().bind(isSelectionEditable);
        selectionRating.editableProperty().bind(isSelectionEditable);
        selectionSubfields.editableProperty().bind(isSelectionEditable);

        selectionSubfieldsPane.prefHeightProperty().bind(selectionAuthorPane.heightProperty());
        selectionAuthorPane.prefHeightProperty().bind(selectionSubfieldsPane.heightProperty());
    }

    private void initialiseFilter() {

        var filterSubfields = new SubfieldPane(model.getSubfields());
        subfieldsFilterPane.setContent(filterSubfields);
        filterSubfields.editableProperty().set(true);

        Util.bindProperties(filterProperties.titleSearchProperty(), titleSearch.textProperty());
        Util.bindProperties(filterProperties.authorSearchProperty(), authorSearch.textProperty());
        Util.bindProperties(filterProperties.minYearProperty(), minYear.textProperty());
        Util.bindProperties(filterProperties.maxYearProperty(), maxYear.textProperty());
        Util.bindProperties(filterProperties.minPagesProperty(), minPages.textProperty());
        Util.bindProperties(filterProperties.maxPagesProperty(), maxPages.textProperty());
        Util.bindProperties(filterProperties.minRatingProperty(), minRatingSlider.valueProperty());
        Util.bindProperties(filterProperties.maxRatingProperty(), maxRatingSlider.valueProperty());
        Util.bindProperties(filterProperties.subfieldsProperty(), filterSubfields.subfieldsProperty());

        filterProperties.clear();

        var min = minRatingSlider.valueProperty();
        var max = maxRatingSlider.valueProperty();
        minRatingSlider.setMin(min.get());
        maxRatingSlider.setMax(max.get());
        minRatingSlider.maxProperty().bind(new When(min.lessThanOrEqualTo(max)).then(max.get()).otherwise(max));
        maxRatingSlider.minProperty().bind(new When(min.lessThanOrEqualTo(max)).then(min.get()).otherwise(min));

        Util.addFormatter(minYear);
        Util.addFormatter(maxYear);
        Util.addFormatter(minPages);
        Util.addFormatter(maxPages);

        titleSearch.setOnAction(event -> applyFilter());
        authorSearch.setOnAction(event -> applyFilter());
        minYear.setOnAction(event -> applyFilter());
        maxYear.setOnAction(event -> applyFilter());
        minPages.setOnAction(event -> applyFilter());
        maxPages.setOnAction(event -> applyFilter());
        minRatingSlider.valueChangingProperty().addListener((observable, oldValue, newValue) -> {
            if (Boolean.FALSE.equals(newValue)) this.applyFilter();
        });
        maxRatingSlider.valueChangingProperty().addListener((observable, oldValue, newValue) -> {
            if (Boolean.FALSE.equals(newValue)) this.applyFilter();
        });

        minRating.textProperty().bind(filterProperties.minRatingProperty().asString());
        maxRating.textProperty().bind(filterProperties.maxRatingProperty().asString());
    }

    private void initialiseButtons() {
        updateButton.textProperty().bind(new When(isUpdating).then("Apply").otherwise("Update"));
        createButton.textProperty().bind(new When(isCreating).then("Apply").otherwise("New"));
    }

    private void initializeBookView() {
        bookView.itemsProperty().bind(model.getLoadedBooks());
        bookView.disableProperty().bind(isSelectionEditable);
        bookView.setCellFactory(param -> new ListCell<>() {
            @Override
            protected void updateItem(Book book, boolean empty) {
                super.updateItem(book, empty);
                if (empty || book == null) setText(null);
                else {
                    prefWidthProperty().bind(param.widthProperty().subtract(15));
                    setPadding(new Insets(5));
                    setWrapText(true);
                    setText(book.title());
                }
            }
        });
        final var sm = bookView.getSelectionModel();
        sm.selectedItemProperty().addListener((observable, oldValue, newValue) -> {
            if (!isSelectionEditable.get() && newValue != null) selectedBook.set(newValue);
        });
        selectedBook.addListener((observable, oldValue, newValue) -> {
            sm.clearSelection();
            sm.select(newValue);
        });

        Platform.runLater(() -> bookView.requestFocus());
    }
}
