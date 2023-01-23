package de.mi.client.controller;

import de.mi.client.model.Model;
import de.mi.common.Book;
import de.mi.common.BookFilter;
import javafx.beans.binding.BooleanBinding;
import javafx.beans.property.SimpleObjectProperty;
import javafx.scene.control.Alert;
import javafx.scene.control.ButtonBar;
import javafx.scene.control.ButtonType;

import java.util.Optional;

@SuppressWarnings({"java:S2211", "java:S1135"}) // TODO: remove suppression
abstract class ControllerBase {
    protected final BookProperties selectedBookProperties = new BookProperties();
    protected final FilterProperties filterProperties = new FilterProperties();
    protected final Model model = new Model();
    protected final SimpleObjectProperty<Book> selectedBook = new SimpleObjectProperty<>();
    private final SimpleObjectProperty<State> state = new SimpleObjectProperty<>(State.NONE);
    protected final BooleanBinding isUpdating = state.isEqualTo(State.UPDATING);
    protected final BooleanBinding isCreating = state.isEqualTo(State.CREATING);
    protected final BooleanBinding isSelectionEditable = isUpdating.or(isCreating);

    protected ControllerBase() {
        isCreating.addListener((observable, oldValue, newValue) -> {
            if (Boolean.TRUE.equals(newValue)) selectedBook.set(null);
        });
        selectedBook.addListener((observable, oldValue, newValue) -> {
            if (newValue != null) selectedBookProperties.set(newValue);
            else selectedBookProperties.clear();
        });
        model.setOnFailAction(() -> {
            state.set(State.NONE);
            selectedBook.set(null);
        });
    }

    private static <X extends Throwable> String getCauseMsg(X e) {
        return Optional.of(e)
                .map(Throwable::getCause)
                .map(Throwable::getMessage)
                .map("%nReason: %s"::formatted)
                .orElse("");
    }

    public abstract void initialize();

    public final SimpleObjectProperty<String> getAppName() {
        return model.getProgrammName();
    }

    protected final void selectionAction(boolean isUpdate, boolean isDelete, boolean isCreate) {
        try {
            if (isUpdate) updateBook();
            else if (isDelete) deleteBook();
            else if (isCreate) createBook();
            else throw new IllegalArgumentException("Action is neither of update, delete or create");
        } catch (Util.PropertyException e) {

            if (state.get() == State.DELETING) {
                var alert = new Alert(Alert.AlertType.WARNING);
                alert.setHeaderText("Can't perform delete action due to invalid value(s)");
                alert.setContentText(String.format("Cause: %s%s",
                        e.getMessage(),
                        getCauseMsg(e)));
                alert.showAndWait();
                state.set(State.NONE);
            } else {
                var alert = new Alert(
                        Alert.AlertType.WARNING,
                        null,
                        new ButtonType("Abort", ButtonBar.ButtonData.OK_DONE),
                        new ButtonType("Continue", ButtonBar.ButtonData.CANCEL_CLOSE));
                alert.setHeaderText("Invalid Book Value(s)");
                alert.setContentText(String.format("For: %s%s%nClear values and abort action?",
                        e.getMessage(),
                        getCauseMsg(e)));
                alert.showAndWait().ifPresent((ButtonType type) -> {
                    if (type.getButtonData() == ButtonBar.ButtonData.OK_DONE) {
                        state.set(State.NONE);
                        selectedBook.set(null);
                    }
                });
            }

        }
    }

    protected final void loadBooks(boolean filter) {
        if (filter) {
            if (state.get() == State.NONE) {
                selectedBook.set(null);
                getFilter().ifPresent(model::loadBooks);
            } else alertCantPerformAction("applying filter");
        } else {
            model.loadBooks();
            filterProperties.clear();
        }
    }

    private void updateBook() {
        switch (state.get()) {
            case NONE -> state.set(State.UPDATING);
            case UPDATING -> {
                model.updateBook(selectedBookProperties.get());
                state.set(State.NONE);
                loadBooks(false);
            }
            default -> alertCantPerformAction(State.UPDATING.toString());
        }
    }

    private void createBook() {
        switch (state.get()) {
            case NONE -> state.set(State.CREATING);
            case CREATING -> {
                model.createBook(selectedBookProperties.get());
                state.set(State.NONE);
                loadBooks(false);
            }
            default -> alertCantPerformAction(State.CREATING.toString());
        }
    }

    private void deleteBook() {
        if (state.get() == State.NONE) {
            state.set(State.DELETING);
            var alert = new Alert(
                    Alert.AlertType.CONFIRMATION,
                    null,
                    new ButtonType("Delete", ButtonBar.ButtonData.OK_DONE),
                    new ButtonType("Cancel", ButtonBar.ButtonData.CANCEL_CLOSE));
            alert.setHeaderText("Confirm Deletion");
            alert.setContentText("Should the book '" +
                                 selectedBookProperties.titleProperty().get() +
                                 "' really be deleted?");
            alert.showAndWait()
                    .map(ButtonType::getButtonData)
                    .map(t -> t.isDefaultButton() ? selectedBookProperties.get() : null)
                    .ifPresent(model::deleteBook);
            state.set(State.NONE);
            selectedBook.set(null);
            loadBooks(false);
        } else alertCantPerformAction(State.DELETING.toString());
    }

    private Optional<BookFilter> getFilter() {
        try {
            return Optional.of(filterProperties.get());
        } catch (Util.PropertyException e) {

            var alert = new Alert(
                    Alert.AlertType.WARNING,
                    null,
                    new ButtonType("Abort", ButtonBar.ButtonData.OK_DONE),
                    new ButtonType("Continue", ButtonBar.ButtonData.CANCEL_CLOSE));
            alert.setHeaderText("Invalid Filter Value(s)");
            alert.setContentText(String.format("For: %s%s%nClear values and abort action?",
                    e.getMessage(),
                    getCauseMsg(e)));
            alert.showAndWait().ifPresent((ButtonType type) -> {
                if (type.getButtonData() == ButtonBar.ButtonData.OK_DONE) {
                    filterProperties.clear();
                    loadBooks(false);
                }
            });

            return Optional.empty();
        }
    }

    private void alertCantPerformAction(String reason) {
        var alert = new Alert(Alert.AlertType.INFORMATION);
        alert.setHeaderText("Action can currently not be performed");
        Optional.ofNullable(reason)
                .map("%%s during %s is not allowed".formatted(state.get())::formatted)
                .map(s -> Character.toUpperCase(s.charAt(0)) + s.substring(1))
                .ifPresent(alert::setContentText);
        alert.showAndWait();
    }

    private enum State {
        NONE, UPDATING, DELETING, CREATING;

        @Override
        public String toString() {
            return switch (this) {
                case NONE -> null;
                case UPDATING -> "book editing";
                case DELETING -> "book deletion";
                case CREATING -> "book creation";
            };
        }
    }
}
