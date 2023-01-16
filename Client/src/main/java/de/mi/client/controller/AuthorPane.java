package de.mi.client.controller;

import de.mi.common.Author;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.ListProperty;
import javafx.beans.property.SetProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleListProperty;
import javafx.beans.property.SimpleSetProperty;
import javafx.collections.FXCollections;
import javafx.collections.ListChangeListener;
import javafx.collections.SetChangeListener;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.function.IntSupplier;

@SuppressWarnings("java:S2211") // TODO: remove suppression
class AuthorPane extends VBox {
    private static final double SPACING = 10D;
    private final ListProperty<AuthorProperties> authors
            = new SimpleListProperty<>(FXCollections.observableArrayList());
    private final BooleanProperty editableProperty = new SimpleBooleanProperty();

    @SuppressWarnings("java:S3776") // TODO: remove suppression
    public AuthorPane() {
        super(SPACING);
        final var children = super.getChildren();
        final Button addNewAuthorButton = new Button("Add");

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
                        if (child instanceof AuthorField f &&
                            c.getRemoved().contains(f.authorProperties)) {
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

    public ListProperty<AuthorProperties> authorProperties() {
        return authors;
    }

    public BooleanProperty editableProperty() {
        return editableProperty;
    }

    private final class AuthorField extends HBox {
        private final AuthorProperties authorProperties;

        private AuthorField(AuthorProperties authorProperties) {
            this.authorProperties = authorProperties;
            final Label authorLabel = new Label();
            final TextField firstNameField = new TextField();
            final TextField lastNameField = new TextField();
            final Button removeButton = new Button("X");
            final var children = super.getChildren();

            editableProperty.addListener((observable, oldValue, newValue) -> {
                children.clear();
                if (Boolean.FALSE.equals(newValue)) children.add(authorLabel);
                else children.addAll(firstNameField, lastNameField, removeButton);
            });

            if (editableProperty.get()) children.addAll(firstNameField, lastNameField, removeButton);
            else children.add(authorLabel);

            removeButton.onActionProperty().set(event -> authors.remove(authorProperties));
            firstNameField.setPromptText("first name");
            lastNameField.setPromptText("last name");
            firstNameField.textProperty().bindBidirectional(authorProperties.firstNameProperty());
            lastNameField.textProperty().bindBidirectional(authorProperties.lastNameProperty());
            authorLabel.textProperty().bind(firstNameField.textProperty()
                    .concat(' ')
                    .concat(lastNameField.textProperty()));
        }
    }
}
