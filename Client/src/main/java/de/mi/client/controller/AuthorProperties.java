package de.mi.client.controller;

import de.mi.client.parser.PropertyParser;
import de.mi.common.Author;
import javafx.beans.property.IntegerProperty;
import javafx.beans.property.SimpleIntegerProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;

class AuthorProperties {
    private final IntegerProperty idProperty
            = new SimpleIntegerProperty(this, "Author id");
    private final StringProperty firstNameProperty
            = new SimpleStringProperty(this, "Author first name");
    private final StringProperty lastNameProperty
            = new SimpleStringProperty(this, "Author last name");

    public AuthorProperties(Author author) {
        idProperty.set(author.id());
        firstNameProperty.set(author.firstName());
        lastNameProperty.set(author.lastName());
    }

    public Author get() {
        int id = PropertyParser.parseInteger(idProperty).getOrThrow();
        String firstName = PropertyParser.parseString(firstNameProperty).getOrThrow();
        String lastName = PropertyParser.parseString(lastNameProperty).getOrThrow();

        return new Author(id, firstName, lastName);
    }

    public StringProperty firstNameProperty() {
        return firstNameProperty;
    }

    public StringProperty lastNameProperty() {
        return lastNameProperty;
    }
}
