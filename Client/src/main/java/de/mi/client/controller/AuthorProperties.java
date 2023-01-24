package de.mi.client.controller;

import de.mi.common.Author;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;

/**
 * Klasse, welche die Werte eines {@link Author Autors}
 * mittels {@link javafx.beans.property.Property Properties} darstellt
 *
 * @author Malte Kasolowsky <code>m30114</code>
 */
class AuthorProperties {
    private final int id;
    private final StringProperty firstNameProperty
            = new SimpleStringProperty(this, "Author first name");
    private final StringProperty lastNameProperty
            = new SimpleStringProperty(this, "Author last name");

    /**
     * Konstruktor; initialisiert die unterliegenden {@link javafx.beans.property.Property Properties}
     * mit den Namen des übergebenen {@link Author Autors} und speichert die Autor-ID
     *
     * @param author Der Autor, mit welchem die ID und Properties initialisiert werden sollen
     */
    public AuthorProperties(Author author) {
        id = author.id();
        firstNameProperty.set(author.firstName());
        lastNameProperty.set(author.lastName());
    }

    /**
     * Liest die aktuellen Werte der unterliegenden {@link javafx.beans.property.Property Properties} aus
     * und erzeugt daraus einen neuen {@link Author}
     *
     * @return Einen neuen Autor mit den Werten der Properties und der initial gespeicherten ID
     * @throws Util.PropertyException Wenn der Vor- oder Nachname leer ist
     */
    public Author get() throws Util.PropertyException {
        String firstName = Util.readProperty(firstNameProperty);
        String lastName = Util.readProperty(lastNameProperty);

        return new Author(id, firstName, lastName);
    }

    /**
     * Getter für den Wert des Vornamens des {@link Author Autors}
     *
     * @return Eine {@link javafx.beans.property.Property}, welche den Wert des Autor-Vornamens trägt
     */
    public StringProperty firstNameProperty() {
        return firstNameProperty;
    }

    /**
     * Getter für den Wert des Nachnamens des {@link Author Autors}
     *
     * @return Eine {@link javafx.beans.property.Property}, welche den Wert des Autor-Nachnamens trägt
     */
    public StringProperty lastNameProperty() {
        return lastNameProperty;
    }
}
