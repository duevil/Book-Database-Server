package de.mi.client.controller;

import de.mi.common.Author;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.ListProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleListProperty;
import javafx.collections.FXCollections;
import javafx.collections.ListChangeListener;
import javafx.scene.Cursor;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;

import java.util.LinkedList;

/**
 * Implementiert eine {@link VBox} mit einer Liste aus {@link Author Autoren},
 * welche je nach Zustand geändert entfernt und hinzugefügt werden können
 *
 * @author Malte Kasolowsky <code>m30114</code>
 */
@SuppressWarnings({"java:S2211", "java:S2972", "java:S134", "java:S3776", "java:S1135"}) // TODO: remove suppression
class AuthorPane extends VBox {
    private static final double SPACING = 10D;
    private final ListProperty<AuthorProperties> authors
            = new SimpleListProperty<>(FXCollections.observableArrayList());
    private final BooleanProperty editableProperty = new SimpleBooleanProperty();

    /**
     * Konstruktor; initialisiert die geerbte {@link VBox} sowie den Inhalt
     */
    public AuthorPane() {
        super(SPACING);
        final var children = super.getChildren();
        final Button addNewAuthorButton = new Button("Add author");
        addNewAuthorButton.setCursor(Cursor.HAND);

        authors.addListener((ListChangeListener<? super AuthorProperties>) c -> {
            while (c.next()) {
                if (c.wasAdded()) {
                    children.remove(addNewAuthorButton);
                    children.addAll(c.getAddedSubList().stream().map(AuthorField::new).toList());
                    if (editableProperty.get()) children.add(addNewAuthorButton);
                }
                if (c.wasRemoved()) {
                    var removed = new LinkedList<AuthorField>();
                    for (Node child : children) {
                        if (child instanceof AuthorField f && c.getRemoved().contains(f.authorProperties)) {
                            removed.add(f);
                        }
                    }
                    children.removeAll(removed);
                }
            }
        });

        editableProperty.addListener((observable, oldValue, newValue) -> {
            if (Boolean.TRUE.equals(newValue)) children.add(addNewAuthorButton);
            else children.remove(addNewAuthorButton);
        });

        addNewAuthorButton.setOnAction(event -> {
            Author author = new Author(0, null, null);
            authors.add(new AuthorProperties(author));
        });
    }

    /**
     * Getter für die gespeicherten {@link Author Autoren}
     *
     * @return Eine {@link ListProperty} mit den momenten angezeigten Autoren
     */
    public ListProperty<AuthorProperties> authorProperties() {
        return authors;
    }

    /**
     * Getter für den Zustand des Veränderbarkeit des Elements
     *
     * @return Die {@link BooleanProperty}, welche anzeigt, ob die Autoren bearbeitet werden können oder nicht
     */
    public BooleanProperty editableProperty() {
        return editableProperty;
    }

    /**
     * Innere Klasse, welche {@link AuthorProperties} speichert und diese bei Veränderbarkeit des Hauptelementes
     * über zwei {@link TextField Textfelder} veränderbar macht
     */
    private final class AuthorField extends HBox {
        private final AuthorProperties authorProperties;

        /**
         * Konstruktor; initialisiert den Inhalt des Elements
         *
         * @param authorProperties Die {@link AuthorProperties}, welche an die Textfelder gebunden werden
         */
        private AuthorField(AuthorProperties authorProperties) {
            this.authorProperties = authorProperties;
            final Label authorLabel = new Label();
            final TextField firstName = new TextField();
            final TextField lastName = new TextField();
            final Button removeButton = new Button("X");
            final var children = super.getChildren();

            editableProperty.addListener((observable, oldValue, newValue) -> {
                children.clear();
                if (Boolean.FALSE.equals(newValue)) children.add(authorLabel);
                else children.addAll(firstName, lastName, removeButton);
            });

            removeButton.setOnAction(event -> authors.remove(authorProperties));
            removeButton.setCursor(Cursor.HAND);
            firstName.setPromptText("first name");
            lastName.setPromptText("last name");
            firstName.textProperty().bindBidirectional(authorProperties.firstNameProperty());
            lastName.textProperty().bindBidirectional(authorProperties.lastNameProperty());
            authorLabel.textProperty().bind(firstName.textProperty().concat(' ').concat(lastName.textProperty()));

            if (editableProperty.get()) children.addAll(firstName, lastName, removeButton);
            else children.add(authorLabel);
        }
    }
}
