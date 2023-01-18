package de.mi.client.controller;

import de.mi.common.Author;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;

class AuthorProperties {
    private final int id;
    private final StringProperty firstNameProperty
            = new SimpleStringProperty(this, "Author first name");
    private final StringProperty lastNameProperty
            = new SimpleStringProperty(this, "Author last name");

    public AuthorProperties(Author author) {
        id = author.id();
        firstNameProperty.set(author.firstName());
        lastNameProperty.set(author.lastName());
    }

    public Author get() throws Util.PropertyException {
        String firstName = Util.readProperty(firstNameProperty);
        String lastName = Util.readProperty(lastNameProperty);

        return new Author(id, firstName, lastName);
    }

    public StringProperty firstNameProperty() {
        return firstNameProperty;
    }

    public StringProperty lastNameProperty() {
        return lastNameProperty;
    }
}
