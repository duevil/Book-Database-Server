package de.mi.client.controller;

import de.mi.common.Subfield;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SetProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleSetProperty;
import javafx.collections.FXCollections;
import javafx.collections.SetChangeListener;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Label;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;

import java.util.HashMap;
import java.util.Set;
import java.util.stream.Stream;

@SuppressWarnings("java:S2211") // TODO: remove suppression
class SubfieldPane extends VBox {
    private static final double SPACING = 10D;
    private final SetProperty<Subfield> subfieldsProperty
            = new SimpleSetProperty<>(FXCollections.observableSet());
    private final BooleanProperty editableProperty = new SimpleBooleanProperty();
    private final Set<Subfield> options;

    @SuppressWarnings("java:S3776") // TODO: remove suppression
    public SubfieldPane(Set<Subfield> options) {
        super(SPACING);
        this.options = options;
        final var children = super.getChildren();
        final Button addNewSelectionButton = new Button("Add");

        subfieldsProperty.addListener((SetChangeListener<? super Subfield>) c -> {
            if (c.wasAdded()) {
                boolean alreadyExists = fieldsForSubfield(c.getElementAdded()).findFirst().isPresent();
                if (!alreadyExists) {
                    children.remove(addNewSelectionButton);
                    children.add(new SubfieldField(c.getElementAdded()));
                    if (editableProperty.get()) children.add(addNewSelectionButton);
                }
            }
            if (c.wasRemoved()) {
                fieldsForSubfield(c.getElementRemoved()).findFirst().ifPresent(children::remove);
            }
        });

        editableProperty.addListener((observable, oldValue, newValue) -> {
            var map = new HashMap<Comparable<?>, SubfieldField>();
            for (Node child : children) {
                if (child instanceof SubfieldField subfieldField) {
                    map.put(subfieldField.subfieldProperty.get(), subfieldField);
                }
            }
            children.clear();
            children.addAll(map.values());
            if (Boolean.TRUE.equals(newValue)) children.add(addNewSelectionButton);
        });

        addNewSelectionButton.setOnAction(event -> {
            for (Subfield option : this.options) {
                if (!subfieldsProperty.contains(option)) {
                    subfieldsProperty.add(option);
                    break;
                }
            }
        });
    }

    private Stream<SubfieldField> fieldsForSubfield(Subfield search) {
        return super.getChildren()
                .stream()
                .filter(SubfieldField.class::isInstance)
                .map(SubfieldField.class::cast)
                .filter(s -> s.subfieldProperty.get().equals(search));
    }

    public SetProperty<Subfield> subfieldsProperty() {
        return subfieldsProperty;
    }

    public BooleanProperty editableProperty() {
        return editableProperty;
    }

    public void reset() {
        this.subfieldsProperty.clear();
        this.subfieldsProperty.addAll(options);
    }

    @SuppressWarnings("java:S2972") // TODO: remove suppression
    private final class SubfieldField extends HBox {
        private final ObjectProperty<Subfield> subfieldProperty;

        @SuppressWarnings("java:S3366") // TODO: remove suppression
        private SubfieldField(Subfield subfield) {
            var items = FXCollections.observableArrayList(options);
            final ComboBox<Subfield> comboBox = new ComboBox<>(items);

            subfieldProperty = comboBox.valueProperty();
            subfieldProperty.setValue(subfield);
            subfieldProperty.addListener((observable, oldValue, newValue) -> {
                subfieldsProperty.add(newValue);
                boolean multiple = fieldsForSubfield(oldValue).anyMatch(s -> s != this);
                if (!multiple) subfieldsProperty.remove(oldValue);
            });

            final Button removeButton = new Button("X");
            removeButton.setOnAction(event -> {
                boolean multiple = fieldsForSubfield(subfieldProperty.get()).anyMatch(s -> s != this);
                if (multiple) SubfieldPane.super.getChildren().remove(this);
                else subfieldsProperty.remove(subfieldProperty.get());
            });

            final Label label = new Label(subfield.toString());
            final var children = super.getChildren();

            editableProperty.addListener((observable, oldValue, newValue) -> {
                children.clear();
                if (Boolean.FALSE.equals(newValue)) children.add(label);
                else children.addAll(comboBox, removeButton);
            });

            if (editableProperty.get()) children.addAll(comboBox, removeButton);
            else children.add(label);
        }
    }
}
