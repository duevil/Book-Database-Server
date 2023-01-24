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

/**
 * Basisklasse für {@link Controller}; dient in erster Stelle dazu, das Gewicht der Controller-Klasse zu reduzieren
 * und dafür zu sorgen, dass diese nur die Initialisierung und {@link javafx.fxml.FXML}-Felder und -Methoden beinhaltet
 * <p>
 * Für die Server-Kommunikation und dem Lesen und Schreiben von (Buch-)Daten wird ein bei der Instanziierung
 * neu erstelltes {@link Model} genutzt
 *
 * @author Malte Kasolowsky <code>m30114</code>
 */
abstract class ControllerBase {
    protected final BookProperties selectedBookProperties = new BookProperties();
    protected final FilterProperties filterProperties = new FilterProperties();
    protected final Model model = new Model();
    protected final SimpleObjectProperty<Book> selectedBook = new SimpleObjectProperty<>();
    private final SimpleObjectProperty<State> state = new SimpleObjectProperty<>(State.NONE);
    protected final BooleanBinding isUpdating = state.isEqualTo(State.UPDATING);
    protected final BooleanBinding isCreating = state.isEqualTo(State.CREATING);
    protected final BooleanBinding isSelectionEditable = isUpdating.or(isCreating);

    /**
     * Konstruktor; initialisiert {@link javax.swing.event.ChangeListener}
     * für einige {@link javafx.beans.property.Property Properties}
     */
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

    /**
     * Utility-Methode zum Umformen der {@link Throwable#getCause() Ursache} einer Ausnahme in einen String
     *
     * @param e   Eine Ausnahme, dessen Ursache ausgelesen werden soll
     * @param <X> Der Typ der Ausnahme; muss {@link Throwable} erweitern
     * @return Einen String mit der {@link Throwable#getCause() Nachricht} der Ursache
     */
    private static <X extends Throwable> String getCauseMsg(X e) {
        return Optional.of(e)
                .map(Throwable::getCause)
                .map(Throwable::getMessage)
                .map("%nReason: %s"::formatted)
                .orElse("");
    }

    /**
     * Abstrakte Methode zum Initialisieren des Controllers und der View; sollte beim Laden der View aufgerufen werden
     */
    public abstract void initialize();

    /**
     * Gibt den Namen des Programms, welcher durch das {@link Model} {@link Model#getProgrammName()} geladen wurde
     *
     * @return Eine {@link javafx.beans.property.Property} mit dem Namen der Anwendung
     */
    public final SimpleObjectProperty<String> getAppName() {
        return model.getProgrammName();
    }

    /**
     * Methode zum Bearbeiten des ausgewählten {@link Book Buches};
     * je nach gewählter Aktion wird diese so fern möglich ausgeführt
     * und dabei entstehende Ausnahmen abgefangen und mittels eines {@link Alert Alerts} angezeigt
     *
     * @param isUpdate Ob das ausgewählte Buch ge-updated werden soll
     * @param isDelete Ob das ausgewählte Buch gelöscht werden soll
     * @param isCreate Ob ein neues Buch erstellt werden soll
     */
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

    /**
     * Lädt die {@link Book Bücher aus der Datenbank};
     * ist <i>filter</i> auf true, so werden die eingegebene Filter {@link ControllerBase#getFilter() ausgelesen}
     * und die Bücher mit {@link Model#loadBooks(BookFilter)} gefiltert geladen,
     * sofern der {@link State} des Controllers auf {@link State#NONE} steht,
     * ansonsten wird eine entsprechende {@link ControllerBase#alertCantPerformAction(String) Fehlermeldung} angezeigt;
     * ist <i>filter</i> auf false, so werden alle Bücher mit {@link Model#loadBooks()} geladen
     *
     * @param filter Ob die Bücher gefiltert geladen werden sollen oder nicht
     */
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

    /**
     * Updated die Werte des ausgewählten {@link Book Buches};
     * ist der {@link State} des Controllers auf {@link State#NONE}, so wird dieser auf {@link State#UPDATING}
     * gesetzt, ist er auf {@link State#UPDATING}, so werden die aktuellen Werte der {@link BookProperties}
     * ausgelesen und die Werte des ausgewählten Buches mit {@link Model#updateBook(Book)} ge-updatet
     * <p>
     * Wenn der {@link State} des Controllers nicht {@link State#NONE} ist,
     * so wird eine {@link ControllerBase#alertCantPerformAction(String) Fehlermeldung angezeigt}
     */
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

    /**
     * Erstellt ein neues {@link Book} aus den eingegebenen Werten;
     * ist der {@link State} des Controllers auf {@link State#NONE}, so wird dieser auf {@link State#CREATING}
     * gesetzt, ist er auf {@link State#CREATING}, so werden die aktuellen Werte der {@link BookProperties}
     * ausgelesen und das erzeugte Buch mit {@link Model#createBook(Book)} erstellt
     * <p>
     * Wenn der {@link State} des Controllers nicht {@link State#NONE} ist,
     * so wird eine {@link ControllerBase#alertCantPerformAction(String) Fehlermeldung angezeigt}
     */
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

    /**
     * Löscht das momentan ausgewählte {@link Book};
     * zeigt ein {@link Alert} zur Bestätigung des Löschens,
     * liest bei Bestätigung die aktuellen Werte der {@link BookProperties} aus
     * und löscht dieses mit {@link Model#deleteBook(Book)}
     * <p>
     * Wenn der {@link State} des Controllers nicht {@link State#NONE} ist,
     * so wird eine {@link ControllerBase#alertCantPerformAction(String) Fehlermeldung angezeigt}
     */
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

    /**
     * Liest den {@link FilterProperties#get() Filter} aus
     * und zeigt ein {@link Alert} mit der Fehlermeldung, sollte beim Auslesen eine geworfen werden
     *
     * @return Ein {@link Optional}, welches einen {@link BookFilter} enthält,
     * sollte dieser erfolgreich ausgelesen worden sein, ansonsten ein {@link Optional#empty() leeres Optional}
     */
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

    /**
     * Erstellt und zeigt ein {@link Alert} mit dem Hinweis, das eine Aktion ausgeführt werden kann
     * und einem optionalen Grund
     *
     * @param reason Der Grund, warum die Aktion nicht ausgeführt werden kann; kann null sein
     */
    private void alertCantPerformAction(String reason) {
        var alert = new Alert(Alert.AlertType.INFORMATION);
        alert.setHeaderText("Action can currently not be performed");
        Optional.ofNullable(reason)
                .map("%%s during %s is not allowed".formatted(state.get())::formatted)
                .map(s -> Character.toUpperCase(s.charAt(0)) + s.substring(1))
                .ifPresent(alert::setContentText);
        alert.showAndWait();
    }

    /**
     * Enum für den Zustand des Controllers, genauer gesagt den Zustand der Bearbeitung des ausgewählten Buches
     */
    private enum State {
        NONE, UPDATING, DELETING, CREATING;

        /**
         * Erzeugt String-Repräsentation des Zustandes;
         * ist der Zustand {@link State#NONE}, so ist der String null
         *
         * @return Eine String-Repräsentation des Zustandes
         */
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
