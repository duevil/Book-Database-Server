package de.mi.client.controller;

import de.mi.client.parser.ParseResult;
import de.mi.client.parser.PropertyParser;
import de.mi.common.Author;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;

class AuthorProperties {
    private static int count;
    private final int id;
    private final StringProperty firstNameProperty
            = new SimpleStringProperty(this, "Author first name");
    private final StringProperty lastNameProperty
            = new SimpleStringProperty(this, "Author last name");

    public AuthorProperties(Author author) {
        firstNameProperty.set(author.firstName());
        lastNameProperty.set(author.lastName());
        id = count++;
    }

    public Author getOrThrow() {
        return get(true);
    }

    public Author get() {
        return get(false);
    }

    private Author get(boolean check) {
        ParseResult<String> parsedFirstName = PropertyParser.parseString(firstNameProperty);
        ParseResult<String> parsedLastName = PropertyParser.parseString(lastNameProperty);
        String firstName = check ? parsedFirstName.getOrThrow() : parsedFirstName.get();
        String lastName = check ? parsedLastName.getOrThrow() : parsedLastName.get();

        return new Author(firstName, lastName);
    }

    public StringProperty firstNameProperty() {
        return firstNameProperty;
    }

    public StringProperty lastNameProperty() {
        return lastNameProperty;
    }
}
