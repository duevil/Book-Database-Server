package de.mi.server;

import de.mi.common.Author;
import de.mi.common.Book;
import de.mi.common.Subfield;
import de.mi.server.sql.Executor;
import de.mi.server.sql.ExecutorFactory;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.sql.PreparedStatement;
import java.sql.SQLException;

/**
 * Utility-Klasse mit Funktionen zum Manipulieren von Literatur-Daten aus der Datenbank (DML)
 *
 * @author Malte Kasolowsky <code>m30114</code>
 */
public final class LiteratureUpdater {
    private static final Logger LOGGER = LogManager.getLogger(LiteratureUpdater.class);

    /**
     * Privater Konstruktor; eine Erzeugung einer Klassen-Instanz ist nicht nötig
     */
    private LiteratureUpdater() {
    }

    /**
     * Updated das gegebene {@link Book} mit den geänderten Werten
     *
     * @param book Das Buch mit den neuen, upzudatenden Werten
     * @throws SQLException             Wenn beim Ausführen der Aktion eine solche Ausnahme geworfen wird
     * @throws IllegalArgumentException Wann das zu updatende Buch nicht in der Datenbank existiert
     */
    public static void updateBook(Book book) throws SQLException, IllegalArgumentException {
        LOGGER.info("Updating a book on the database: {}", book);

        LOGGER.trace("Loading old book from database...");
        var old = LiteratureQuery.queryBooks().stream().filter(b -> b.equals(book)).findFirst();
        LOGGER.trace("Loaded old book from database: {}", old);
        if (old.isEmpty()) {
            LOGGER.warn("Can not update a book that does not exist: {}", book);
            throw new IllegalArgumentException("Book does not exist and thus can not be updated");
        }

        var values = new Object[] {book.title(), book.publisher(), book.year(), book.pages(), book.rating(), book.id()};
        LOGGER.trace("Updating book with values: {}", values);
        Queries.UPDATE_BOOK.execute(values);

        LOGGER.trace("Updating book authors...");
        for (Author author : book.authors()) {
            if (author.id() == 0) insertAuthor(book, author);
            else {
                Queries.UPDATE_AUTHOR.execute(author.firstName(), author.lastName(), author.id());
                Queries.INSERT_BOOK_AUTHORS.execute(book.id(), author.id());
            }
        }

        LOGGER.trace("Updating book subfields...");
        for (Subfield subfield : book.subfields()) {
            Queries.INSERT_BOOK_SUBFIELDS.execute(book.id(), subfield.id());
        }

        LOGGER.trace("Deleting old book authors and subfields...");
        for (Author author : old.get().authors()) {
            if (!book.authors().contains(author)) Queries.DELETE_BOOK_AUTHOR.execute(book.id(), author.id());
        }
        Queries.DELETE_AUTHORS.execute();

        LOGGER.trace("Deleting old book subfields...");
        for (Subfield subfield : old.get().subfields()) {
            if (!book.subfields().contains(subfield)) Queries.DELETE_BOOK_SUBFIELD.execute(book.id(), subfield.id());
        }

        LOGGER.info("Potentially updated a book on the database: {}", book);
    }

    /**
     * Löscht ein {@link Book} aus der Datenbank
     *
     * @param id Die ID des Buches, welches gelöscht werden soll
     * @throws SQLException Wenn beim Ausführen der Aktion eine solche Ausnahme geworfen wird
     */
    public static void deleteBook(int id) throws SQLException {
        LOGGER.info("Deleting a book from the database with the id [{}]", id);
        LOGGER.trace("Deleting book...");
        Queries.DELETE_BOOK.execute(id);
        LOGGER.trace("Deleting book authors...");
        Queries.DELETE_BOOK_AUTHORS_ALL.execute(id);
        LOGGER.trace("Deleting book subfields...");
        Queries.DELETE_BOOK_SUBFIELDS_ALL.execute(id);
        LOGGER.trace("Deleting authors...");
        Queries.DELETE_AUTHORS.execute();

        LOGGER.info("Potentially deleted a book from the database with the id [{}]", id);
    }

    /**
     * Fügt ein neues {@link Book} in die Datenbank ein
     *
     * @param book Das Buch, das eingefügt werden soll
     * @throws SQLException             Wenn beim Ausführen der Aktion eine solche Ausnahme geworfen wird
     * @throws IllegalArgumentException Wenn das einzufügende Buch bereits existiert
     */
    public static void insertBook(Book book) throws SQLException, IllegalArgumentException {
        LOGGER.info("Inserting a new book into the database: {}", book);
        if (book.id() != 0 || LiteratureQuery.queryBooks().contains(book)) {
            LOGGER.warn("Can not insert a book that already exists: {}", book);
            throw new IllegalArgumentException("Book does already exist and thus can not be inserted");
        }

        var values = new Object[] {book.title(), book.publisher(), book.year(), book.pages(), book.rating()};
        LOGGER.trace("Inserting book with values: {}", values);
        Queries.INSERT_BOOK.execute(values);
        Book inserted = LiteratureQuery.queryBookByTitle(book.title());

        LOGGER.trace("Inserting book authors...");
        for (Author author : book.authors()) {
            if (author.id() == 0) insertAuthor(inserted, author);
            else Queries.INSERT_BOOK_AUTHORS.execute(inserted.id(), author.id());
        }

        LOGGER.trace("Inserting book subfields...");
        for (Subfield subfield : book.subfields()) {
            Queries.INSERT_BOOK_SUBFIELDS.execute(inserted.id(), subfield.id());
        }

        LOGGER.info("Inserted a new book into the database: {}", book);
    }

    /**
     * Fügt einen neuen zu einem {@link Book} gehörenden {@link Author} in die Datenbank ein
     *
     * @param book   Das Buch, zu dem der einzufügende Autor gehört
     * @param author Der Autor, der eingefügt werden soll
     * @throws SQLException Wenn beim Ausführen der Aktion eine solche Ausnahme geworfen wird
     */
    private static void insertAuthor(Book book, Author author) throws SQLException {
        LOGGER.info("Inserting a new author for a book into the database: {} with book {}", author, book);
        LOGGER.trace("Inserting author into database...");
        Queries.INSERT_AUTHOR.execute(author.firstName(), author.lastName());
        Author inserted = LiteratureQuery.queryAuthorByName(author.firstName(), author.lastName());
        if (inserted == null) {
            LOGGER.error("Unable to find inserted author: {}", author);
            throw new IllegalStateException("Unable to find inserted author: " + author);
        }
        LOGGER.trace("Inserting book author into database...");
        Queries.INSERT_BOOK_AUTHORS.execute(book.id(), inserted.id());

        LOGGER.info("Inserted a new author into the database: {}", author);
    }

    /**
     * enum, welches alle {@link Executor} zum Ausführen von einzelnen DML-Befehlen beinhaltet
     */
    private enum Queries implements Executor<Integer> {
        DELETE_AUTHORS("""
                DELETE FROM authors
                WHERE NOT EXISTS(SELECT * FROM book_authors WHERE author_id = id)"""),
        DELETE_BOOK("DELETE FROM books WHERE id = ?"),
        DELETE_BOOK_AUTHOR("DELETE FROM book_authors WHERE book_id = ? AND author_id = ?"),
        DELETE_BOOK_AUTHORS_ALL("DELETE FROM book_authors WHERE book_id = ?"),
        DELETE_BOOK_SUBFIELDS_ALL("DELETE FROM book_subfields WHERE book_id = ?"),
        DELETE_BOOK_SUBFIELD("DELETE FROM book_subfields WHERE book_id = ? AND subfield_id = ?"),
        INSERT_BOOK_AUTHORS("""
                INSERT INTO book_authors (book_id, author_id)
                VALUES (?, ?)
                ON DUPLICATE KEY UPDATE book_id = book_id"""),
        INSERT_BOOK_SUBFIELDS("""
                INSERT INTO book_subfields (book_id, subfield_id)
                VALUES (?, ?)
                ON DUPLICATE KEY UPDATE book_id = book_id"""),
        INSERT_BOOK("""
                INSERT INTO books (title, publisher, year, pages, rating)
                VALUES (?, ?, ?, ?, ?)"""),
        INSERT_AUTHOR("""
                INSERT INTO authors (first_name, last_name)
                VALUES (?, ?)"""),
        UPDATE_BOOK("""
                UPDATE books
                SET title = ?, publisher = ?, year = ?, pages = ?, rating = ?
                WHERE id = ?"""),
        UPDATE_AUTHOR("""
                UPDATE authors
                SET first_name = ?, last_name = ?
                WHERE id = ?""");

        private final Executor<Integer> executor;

        /**
         * Konstruktor; erstellt und speichert einen neuen
         * {@link ExecutorFactory#createUpdater(PreparedStatement) UpdateExecutor} aus dem SQL-Befehl
         *
         * @param sql Der auszuführende SQL-Befehl
         */
        Queries(String sql) {
            executor = ExecutorFactory.createUpdater(DBConnection.prepareStatement(sql));
        }

        /**
         * Führt den gespeicherten {@link Executor} mit den übergebene Argumenten aus
         *
         * @param values Die Parameter für das Statement, sofern dieses ein {@link PreparedStatement} ist
         * @return Die Anzahl an Spalten, die durch das Statement verändert wurden
         * @throws SQLException Wenn beim Ausführen des Befehls eine solche Ausnahme geworfen wird
         */
        @Override
        public Integer execute(Object... values) throws SQLException {
            LOGGER.debug("Executing query: {}", this);
            return executor.execute(values);
        }
    }
}
