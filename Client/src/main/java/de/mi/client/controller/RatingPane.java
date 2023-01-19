package de.mi.client.controller;

import de.mi.common.Book;
import javafx.beans.binding.When;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.IntegerProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleIntegerProperty;
import javafx.scene.Cursor;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.layout.HBox;
import javafx.scene.paint.Color;
import javafx.scene.text.Font;

import java.util.Collection;
import java.util.LinkedList;

@SuppressWarnings({"java:S2211", "java:S1135"}) // TODO: remove suppression
class RatingPane extends HBox {
    private static final Font FONT = Font.font(16D);
    private static final int MAX_RATING = Book.DEFAULT_RATING_RANGE.max();
    private final IntegerProperty ratingProperty = new SimpleIntegerProperty(0);
    private final BooleanProperty editableProperty = new SimpleBooleanProperty();
    private final Collection<Button> buttons = new LinkedList<>();
    private final Collection<Label> labels = new LinkedList<>();

    public RatingPane() {

        for (int j = 1; j <= MAX_RATING; j++) {
            final int rating = j;
            final var label = new Label();
            final var button = new Button("☆");

            label.setFont(FONT);
            button.setCursor(Cursor.HAND);
            button.setOnAction(event -> ratingProperty.set(rating));

            var condition = ratingProperty.greaterThanOrEqualTo(rating);
            button.textProperty().bind(new When(condition).then("★").otherwise("☆"));
            button.textFillProperty().bind(new When(condition).then(Color.valueOf("#039ED3")).otherwise(Color.GREY));
            label.textProperty().bind(button.textProperty());
            label.textFillProperty().bind(button.textFillProperty());

            buttons.add(button);
            labels.add(label);
        }

        final var children = super.getChildren();
        editableProperty.addListener((observable, oldValue, newValue) -> {
            children.clear();
            if (Boolean.TRUE.equals(newValue)) children.addAll(buttons);
            else children.addAll(labels);
        });

        children.addAll(labels);
    }

    public IntegerProperty ratingProperty() {
        return ratingProperty;
    }

    public BooleanProperty editableProperty() {
        return editableProperty;
    }
}
