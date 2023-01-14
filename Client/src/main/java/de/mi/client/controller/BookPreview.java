package de.mi.client.controller;

import de.mi.common.Book;
import javafx.scene.Cursor;
import javafx.scene.control.Label;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.VBox;

import java.util.function.Consumer;

class BookPreview extends VBox {
    private static final double SPACING = 10D;
    private final Consumer<Book> selectionConsumer;

    public BookPreview(final Consumer<Book> onSelection) {
        super(SPACING);
        this.selectionConsumer = onSelection;
    }

    public void setBooks(Iterable<Book> books) {
        final var children = super.getChildren();
        children.clear();
        for (Book book : books) {
            Label label = new Label(book.title());
            label.setWrapText(true);
            label.setCursor(Cursor.HAND);
            label.setOnMousePressed((MouseEvent event) -> {
                if (event.isPrimaryButtonDown()) {
                    this.selectionConsumer.accept(book);
                }
            });
            children.add(label);
        }
    }
}
