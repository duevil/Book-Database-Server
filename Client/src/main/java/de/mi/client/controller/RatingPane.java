package de.mi.client.controller;

import de.mi.common.Book;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.IntegerProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleIntegerProperty;
import javafx.scene.Cursor;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.layout.HBox;
import javafx.scene.text.Font;
import javafx.util.StringConverter;

import java.util.Collection;
import java.util.LinkedList;

@SuppressWarnings({"java:S2211", "java:S1135"}) // TODO: remove suppression
class RatingPane extends HBox {
    private static final int MAX_RATING = Book.DEFAULT_RATING_RANGE.max();
    private static final Font FONT = Font.font(16D);
    private final IntegerProperty ratingProperty = new SimpleIntegerProperty(0);
    private final BooleanProperty editableProperty = new SimpleBooleanProperty();
    private final Collection<Button> buttons = new LinkedList<>();
    private final Label label = new Label();

    public RatingPane() {

        for (int j = 1; j <= MAX_RATING; j++) {
            int i = j;
            final var button = new Button("☆");
            button.setCursor(Cursor.HAND);
            button.setOnAction(event -> ratingProperty.set(i));
            ratingProperty.addListener((observable, oldValue, newValue) -> {
                if (newValue.intValue() >= i) button.setText("★");
                else button.setText("☆");
            });
            buttons.add(button);
        }

        label.setFont(FONT);
        label.textProperty().bindBidirectional(ratingProperty, new StringConverter<>() {
            @Override
            public String toString(Number number) {
                return "☆".repeat(number.intValue());
            }

            @Override
            public Number fromString(String string) {
                return string.length();
            }
        });

        final var children = super.getChildren();
        children.add(label);
        editableProperty.addListener((observable, oldValue, newValue) -> {
            children.clear();
            if (Boolean.TRUE.equals(newValue)) children.addAll(buttons);
            else children.add(label);
        });
    }

    public IntegerProperty ratingProperty() {
        return ratingProperty;
    }

    public BooleanProperty editableProperty() {
        return editableProperty;
    }
}
