package de.mi.client.controller;

import de.mi.common.Author;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.SetProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleSetProperty;
import javafx.collections.FXCollections;
import javafx.collections.SetChangeListener;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;

import java.util.LinkedList;

@SuppressWarnings("java:S2211") // TODO: remove suppression
class AuthorPane extends VBox {
    private static final double SPACING = 10D;
    private final SetProperty<AuthorProperties> authors
            = new SimpleSetProperty<>(FXCollections.observableSet());
    private final BooleanProperty editableProperty = new SimpleBooleanProperty();

    @SuppressWarnings("java:S3776") // TODO: remove suppression
    public AuthorPane() {
        super(SPACING);
        final var children = super.getChildren();
        final Button addNewAuthorButton = new Button("Add");

        authors.addListener((SetChangeListener<? super AuthorProperties>) c -> {
            if (c.wasAdded()) {
                children.remove(addNewAuthorButton);
                children.add(new AuthorField(c.getElementAdded()));
                if (editableProperty.get()) children.add(addNewAuthorButton);
            }
            if (c.wasRemoved()) {
                var remove = new LinkedList<AuthorField>();
                for (Node node : super.getChildren()) {
                    if (node instanceof AuthorField f &&
                        f.authorProperties.equals(c.getElementRemoved())) remove.add(f);
                }
                super.getChildren().removeAll(remove);
            }
        });

        editableProperty.addListener((observable, oldValue, newValue) -> {
            if (Boolean.TRUE.equals(newValue)) children.add(addNewAuthorButton);
            else children.remove(addNewAuthorButton);
        });

        addNewAuthorButton.setOnAction(event -> {
            Author author = new Author("", "");
            authors.add(new AuthorProperties(author));
        });
    }

    public SetProperty<AuthorProperties> authorProperties() {
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
            lastNameField.textProperty().bindBidirectional(authorProperties.lastNameProperty());
            authorLabel.textProperty().bind(firstNameField.textProperty()
                    .concat(' ')
                    .concat(lastNameField.textProperty()));
        }
    }
}
