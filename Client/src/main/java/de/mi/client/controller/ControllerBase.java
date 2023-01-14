package de.mi.client.controller;

import de.mi.client.model.Connection;
import de.mi.client.model.ConnectionFactory;
import de.mi.common.Book;
import de.mi.common.Subfield;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.fxml.FXML;

import java.util.NoSuchElementException;
import java.util.Set;

abstract class ControllerBase {
    protected final Connection connection = ConnectionFactory.create();
    protected final Set<Subfield> subfields = connection.getSubfields().orElseThrow(
            () -> new NoSuchElementException("No subfields loaded")
    );
    protected final FilterProperties filterProperties = new FilterProperties();
    protected final SubfieldPane subfieldFilterSelector = new SubfieldPane(subfields);
    protected final BooleanProperty editable = new SimpleBooleanProperty(false);
    protected final BookProperties selectedBook = new BookProperties();
    protected final BookPreview bookPreview = new BookPreview((Book book) -> {
        if (editable.not().get()) selectedBook.set(book);
    });

    @FXML
    public abstract void initialize();

    public String getAppName() {
        return String.format("%s [%s]", connection.getProgrammName(), connection.getClientType().name());
    }
}
