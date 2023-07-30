package de.mi.client.model;

import de.mi.common.Book;
import de.mi.common.BookFilter;
import de.mi.common.Subfield;
import javafx.beans.property.SimpleListProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.collections.FXCollections;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Model-Klasse des Client-Programms; stellt die Funktionalität zur sicheren Anfrage
 * vom Laden und manipulieren von Daten an den Server
 *
 * @author Malte Kasolowsky <code>m30114</code>
 */
public class Model {
    private final SimpleObjectProperty<String> programmName = new SimpleObjectProperty<>();
    private final SimpleListProperty<Book> loadedBooks = new SimpleListProperty<>(FXCollections.observableArrayList());
    private final ConnectionHandler handler = new ConnectionHandler();
    private final Set<Subfield> subfields = new HashSet<>();
    private Runnable onFailAction;

    /**
     * Konstruktor; lädt den Namen der Anwendung und die {@link Subfield Teilgebiete} vom Server
     */
    public Model() {
        handler.handle(Connection::getProgrammName, programmName::set, null);
        handler.handle(Connection::getSubfields, subfields::addAll, null);
    }

    /**
     * Gibt die initial geladenen {@link Subfield Teilgebiete} zurück
     *
     * @return Ein {@link Collections#unmodifiableSet(Set) un-modifizierbares Set} mit den geladenen Teilgebieten
     */
    public Set<Subfield> getSubfields() {
        return Collections.unmodifiableSet(subfields);
    }

    /**
     * Gibt den initial geladenen Namen der Anwendung als {@link javafx.beans.property.Property} zurück
     *
     * @return Eine Property mit dem Namen der Anwendung
     */
    public SimpleObjectProperty<String> getProgrammName() {
        return programmName;
    }

    /**
     * Getter für die aktuell geladenen {@link Book Bücher}
     *
     * @return Eine {@link javafx.beans.property.Property}, welche die aktuell vom Server geladenen Bücher enthält
     */
    public SimpleListProperty<Book> getLoadedBooks() {
        return loadedBooks;
    }

    /**
     * Lädt die von einem {@link BookFilter} gefilterten {@link Book Bücher} vom Server
     * und überträgt diese in die {@link Model#getLoadedBooks() Property mit den geladenen Büchern}
     *
     * @param filter Der Filter, mit welchem die zu ladenden Bücher gefiltert werden
     */
    public void loadBooks(BookFilter filter) {
        handler.handle(filter, Connection::getBooks, this::setBooks, onFailAction);
    }

    /**
     * Lädt alle {@link Book Bücher} vom Server
     * und überträgt diese in die {@link Model#getLoadedBooks() Property mit den geladenen Büchern}
     */
    public void loadBooks() {
        handler.handle(Connection::getBooks, this::setBooks, onFailAction);
    }

    /**
     * Updated die Werte das gegebene {@link Book}
     *
     * @param book Das zu updatende Buch
     */
    public void updateBook(Book book) {
        handler.handle(book, Connection::updateBook, onFailAction);
    }

    /**
     * Löscht das gegebene {@link Book}
     *
     * @param book Das zu löschende Buch
     */
    public void deleteBook(Book book) {
        handler.handle(book, Connection::deleteBook, onFailAction);
    }

    /**
     * Erstellt ein neues Buch
     *
     * @param book Ein Buch mit den Werten des zu erstellenden
     */
    public void createBook(Book book) {
        handler.handle(book, Connection::createBook, onFailAction);
    }

    /**
     * Speichert ein {@link Runnable}, welches ausgeführt wird, sollte eine Anfrage scheitern
     *
     * @param onFailAction Ein Runnable zum Ausführen beim Scheitern einer Anfrage
     */
    public void setOnFailAction(Runnable onFailAction) {
        this.onFailAction = onFailAction;
    }

    /**
     * Leert die interne Liste mit geladenen {@link Book Büchern} und speicher die übergebenen
     *
     * @param books Die Bücher, welche geladen wurden und gespeichert werden sollen
     */
    private void setBooks(List<Book> books) {
        loadedBooks.clear();
        loadedBooks.setAll(books);
    }
}
