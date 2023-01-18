package de.mi.client.controller;

import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.Slider;
import javafx.scene.control.TextArea;
import javafx.scene.control.TextField;
import javafx.scene.layout.Pane;

@SuppressWarnings({"java:S1820", "java:S3242", "java:S2211", "java:S1135"}) // TODO: remove suppression
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
    public Pane selectionRatingPane;
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
        filterProperties.clear();
        applyFilter(actionEvent);
    }

    @FXML
    public void selectionEvent(ActionEvent event) {
        if (event.getSource() instanceof Button triggerButton)
            selectionAction(triggerButton,
                    triggerButton == updateButton,
                    triggerButton == deleteButton,
                    triggerButton == createButton);
    }

    @FXML
    @Override
    public void initialize() {

        //#region selection initialisation

        var authorPane = new AuthorPane();
        authorSelectionPane.setContent(authorPane);
        var ratingPane = new RatingPane();
        selectionRatingPane.getChildren().add(ratingPane);
        var subfieldPane = new SubfieldPane(subfields);
        subfieldSelectionPane.setContent(subfieldPane);

        Util.bindProperties(selectedBook.titleProperty(), selectionTitle.textProperty());
        Util.bindProperties(selectedBook.authorsProperty(), authorPane.authorProperties());
        Util.bindProperties(selectedBook.publisherProperty(), selectionPublisher.textProperty());
        Util.bindProperties(selectedBook.yearProperty(), selectionYear.textProperty());
        Util.bindProperties(selectedBook.pagesProperty(), selectionPages.textProperty());
        Util.bindProperties(selectedBook.ratingProperty(), ratingPane.ratingProperty());
        Util.bindProperties(selectedBook.subfieldsProperty(), subfieldPane.subfieldsProperty());

        Util.addFormatter(selectionYear);
        Util.addFormatter(selectionPages);

        selectionTitle.editableProperty().bind(editable);
        authorPane.editableProperty().bind(editable);
        selectionPublisher.editableProperty().bind(editable);
        selectionYear.editableProperty().bind(editable);
        selectionPages.editableProperty().bind(editable);
        ratingPane.editableProperty().bind(editable);
        subfieldPane.editableProperty().bind(editable);

        //#endregion
        //#region filter initialisation

        Util.bindProperties(filterProperties.titleSearchProperty(), titleSearch.textProperty());
        Util.bindProperties(filterProperties.authorSearchProperty(), authorSearch.textProperty());
        Util.bindProperties(filterProperties.minYearProperty(), minYear.textProperty());
        Util.bindProperties(filterProperties.maxYearProperty(), maxYear.textProperty());
        Util.bindProperties(filterProperties.minPagesProperty(), minPages.textProperty());
        Util.bindProperties(filterProperties.maxPagesProperty(), maxPages.textProperty());
        Util.bindProperties(filterProperties.minRatingProperty(), minRatingSlider.valueProperty());
        Util.bindProperties(filterProperties.maxRatingProperty(), maxRatingSlider.valueProperty());
        Util.bindProperties(filterProperties.subfieldsProperty(), subfieldFilterSelector.subfieldsProperty());

        filterProperties.clear();

        minRatingSlider.setMin(minRatingSlider.getValue());
        minRatingSlider.setMax(maxRatingSlider.getValue());
        maxRatingSlider.setMin(minRatingSlider.getValue());
        maxRatingSlider.setMax(maxRatingSlider.getValue());

        Util.addFormatter(minYear);
        Util.addFormatter(maxYear);
        Util.addFormatter(minPages);
        Util.addFormatter(maxPages);

        titleSearch.setOnAction(this::applyFilter);
        authorSearch.setOnAction(this::applyFilter);
        minYear.setOnAction(this::applyFilter);
        maxYear.setOnAction(this::applyFilter);
        minPages.setOnAction(this::applyFilter);
        maxPages.setOnAction(this::applyFilter);
        minRatingSlider.valueChangingProperty().addListener((observable, oldValue, newValue) -> {
            if (Boolean.FALSE.equals(newValue)) this.applyFilter(null);
        });
        maxRatingSlider.valueChangingProperty().addListener((observable, oldValue, newValue) -> {
            if (Boolean.FALSE.equals(newValue)) this.applyFilter(null);
        });

        minRating.textProperty().bind(filterProperties.minRatingProperty().asString());
        maxRating.textProperty().bind(filterProperties.maxRatingProperty().asString());

        subfieldFilterSelector.editableProperty().set(true);
        subfieldsFilterPane.setContent(subfieldFilterSelector);

        //#endregion

        previewPane.setContent(bookPreview);

        loadBooks(false);
    }
}
