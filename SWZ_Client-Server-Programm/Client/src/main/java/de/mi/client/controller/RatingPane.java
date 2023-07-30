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
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.Collection;
import java.util.LinkedList;

/**
 * Implementiert eine {@link HBox} zum Anzeigen und Bearbeiten einer Bewertung
 *
 * @author Malte Kasolowsky <code>m30114</code>
 */
class RatingPane extends HBox {
    private static final Logger LOGGER = LogManager.getLogger(RatingPane.class);
    private static final Font FONT = Font.font(16D);
    private static final int MAX_RATING = Book.DEFAULT_RATING_RANGE.max();
    private final IntegerProperty ratingProperty = new SimpleIntegerProperty(0);
    private final BooleanProperty editableProperty = new SimpleBooleanProperty();
    private final Collection<Button> buttons = new LinkedList<>();
    private final Collection<Label> labels = new LinkedList<>();

    /**
     * Konstruktor; initialisiert die vererbte {@link HBox} mit dem Inhalt;
     * je nachdem welcher Wert die {@link RatingPane#editableProperty()} hat,
     * werden {@link Label} mit der aktuellen Bewertung oder {@link Button Buttons}
     * zum Einstellen der Bewertung angezeigt
     */
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
            LOGGER.trace("Editable Property changed: {} -> {}", oldValue, newValue);

            children.clear();
            if (Boolean.TRUE.equals(newValue)) children.addAll(buttons);
            else children.addAll(labels);
        });

        children.addAll(labels);
    }

    /**
     * Getter für den Wert der Bewertung
     *
     * @return Eine {@link IntegerProperty}, welche den Wert der Bewertung enthält
     */
    public IntegerProperty ratingProperty() {
        return ratingProperty;
    }

    /**
     * Getter für den Wert der Veränderbarkeit der Bewertung;
     * ist dieser true, so kann die Bewertung bearbeitet werden, andernfalls nicht
     *
     * @return Eine {@link BooleanProperty}, welche den Wert der Veränderbarkeit der Bewertung enthält
     */
    public BooleanProperty editableProperty() {
        return editableProperty;
    }
}
