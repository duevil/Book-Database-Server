package de.mi.client.controller;

import de.mi.common.Subfield;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SetProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleSetProperty;
import javafx.collections.FXCollections;
import javafx.collections.SetChangeListener;
import javafx.scene.Cursor;
import javafx.scene.control.Button;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Label;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;

import java.util.HashMap;
import java.util.Set;
import java.util.stream.Stream;

/**
 * Implementiert eine {@link VBox} mit einer Liste aus {@link Subfield Teilgebieten},
 * welche je nach Zustand ausgewählt, entfernt oder hinzugefügt werden können
 *
 * @author Malte Kasolowsky <code>m30114</code>
 */
@SuppressWarnings({"java:S2211", "java:S3366", "java:S2972", "java:S3776", "java:S1135"}) // TODO: remove suppression
class SubfieldPane extends VBox {
    private static final double SPACING = 10D;
    private final SetProperty<Subfield> subfieldsProperty
            = new SimpleSetProperty<>(FXCollections.observableSet());
    private final BooleanProperty editableProperty = new SimpleBooleanProperty();
    private final Set<Subfield> options;

    /**
     * Konstruktor; initializer die vererbte {@link VBox} und die eigenen Inhalte
     *
     * @param options Die möglichen {@link Subfield Teilgebiete}, die ausgewählt werden können
     */
    public SubfieldPane(Set<Subfield> options) {
        super(SPACING);
        this.options = options;
        final var children = super.getChildren();
        final Button addNewSelectionButton = new Button("Add subfield");
        addNewSelectionButton.setCursor(Cursor.HAND);

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
            var map = new HashMap<Subfield, SubfieldField>();
            for (var child : children) {
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

    /**
     * Getter für die aktuell ausgewählten {@link Subfield Teilgebiete}
     *
     * @return Eine {@link SetProperty}, welche die aktuell auswählten Teilgebiete enthält
     */
    public SetProperty<Subfield> subfieldsProperty() {
        return subfieldsProperty;
    }

    /**
     * Getter für den Zustand des Veränderbarkeit des Elements
     *
     * @return Die {@link BooleanProperty}, welche anzeigt,
     * ob die Auswahl der Teilgebiete verändert werden kann oder nicht
     */
    public BooleanProperty editableProperty() {
        return editableProperty;
    }

    /**
     * Liest die gespeicherten {@link javafx.scene.Node} aus dem {@link HBox#getChildren() Inhalt der HBox} aus
     * und sucht nach {@link SubfieldField Teilgebiet-Feldern},
     * deren unterliegendes {@link Subfield} einem gesuchten entspricht
     *
     * @param search Das Teilgebiet, welches gesucht werden soll
     * @return Einen {@link Stream}, welcher optimal ein oder kein Teilgebiet-Feld enthält
     */
    private Stream<SubfieldField> fieldsForSubfield(Subfield search) {
        return super.getChildren()
                .stream()
                .filter(SubfieldField.class::isInstance)
                .map(SubfieldField.class::cast)
                .filter(s -> s.subfieldProperty.get().equals(search));
    }

    /**
     * Implementiert eine {@link HBox} zum Anzeigen und Auswählen eines {@link Subfield Teilgebiets}
     * über eine {@link ComboBox}
     */
    private final class SubfieldField extends HBox {
        private final ObjectProperty<Subfield> subfieldProperty;

        /**
         * Konstruktor; initialisiert den Inhalt
         *
         * @param subfield Das initial ausgewählte {@link Subfield}
         */
        private SubfieldField(Subfield subfield) {
            var items = FXCollections.observableArrayList(options);
            final ComboBox<Subfield> comboBox = new ComboBox<>(items);
            comboBox.setCursor(Cursor.HAND);

            subfieldProperty = comboBox.valueProperty();
            subfieldProperty.setValue(subfield);
            subfieldProperty.addListener((observable, oldValue, newValue) -> {
                subfieldsProperty.add(newValue);
                boolean multiple = fieldsForSubfield(oldValue).anyMatch(s -> s != this);
                if (!multiple) subfieldsProperty.remove(oldValue);
            });

            final Button removeButton = new Button("X");
            removeButton.setCursor(Cursor.HAND);
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
