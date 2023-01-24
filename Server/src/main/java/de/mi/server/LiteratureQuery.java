package de.mi.server;

import de.mi.common.Author;
import de.mi.common.Book;
import de.mi.common.BookFilter;
import de.mi.common.Subfield;
import de.mi.server.sql.Executor;
import de.mi.server.sql.ExecutorFactory;
import de.mi.server.sql.Mapper;

import java.sql.SQLException;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Utility-Klasse mit Funktionen zum Laden von Literatur-Daten aus der Datenbank (DRL)
 *
 * @author Malte Kasolowsky <code>m30114</code>
 */
public final class LiteratureQuery {
    private static final Mapper<Book> BOOK_MAPPER = values -> Optional.ofNullable(values.get("id"))
            .map(id -> new Book(
                    (Integer) id,
                    (String) values.get("title"),
                    null,
                    (String) values.get("publisher"),
                    (Integer) values.get("year"),
                    (Integer) values.get("pages"),
                    (Integer) values.get("rating"),
                    null))
            .orElse(new Book(0, null, null, null, -1, -1, -1, null));
    private static final Mapper<Author> AUTHOR_MAPPER = values -> Optional.ofNullable(values.get("id"))
            .map(id -> new Author(
                    (Integer) id,
                    (String) values.get("first_name"),
                    (String) values.get("last_name")))
            .orElse(new Author(0, null, null));
    private static final Mapper<Subfield> SUBFIELD_MAPPER = values -> Optional.ofNullable(values.get("id"))
            .map(id -> new Subfield(
                    (Integer) id,
                    (String) values.get("name")))
            .orElse(new Subfield(0, null));
    private static final BookFilter EMPTY_FILTER = BookFilter.builder().build();
    private static final Set<Subfield> SUBFIELDS;
    private static final String BOOK_SQL = """
            SELECT id, title, publisher, year, pages, rating
            FROM books
            WHERE LOWER(title) LIKE '%%%s%%'
              AND year BETWEEN %d AND %d
              AND pages BETWEEN %d AND %d
              AND rating BETWEEN %d AND %d
              AND EXISTS(SELECT s.id
                         FROM subfields s
                                  JOIN book_subfields bs ON s.id = bs.subfield_id
                         WHERE bs.book_id = books.id
                           AND s.id IN (%s))
              AND EXISTS(SELECT a.id
                         FROM authors a
                                  JOIN book_authors ba ON a.id = ba.author_id
                         WHERE ba.book_id = books.id
                           AND LOWER(CONCAT_WS(' ', a.first_name, a.last_name)) LIKE '%%%s%%')""";
    private static final Executor<List<Author>> BOOK_AUTHOR_EXECUTOR
            = ExecutorFactory.createQuery(DBConnection.prepareStatement("""
            SELECT id, first_name, last_name
            FROM authors JOIN book_authors ON id = author_id
            WHERE book_id = ?"""), AUTHOR_MAPPER);
    private static final Executor<List<Subfield>> BOOK_SUBFIELD_EXECUTOR
            = ExecutorFactory.createQuery(DBConnection.prepareStatement("""
            SELECT id, name
            FROM subfields JOIN book_subfields ON id = subfield_id
            WHERE book_id = ?"""), SUBFIELD_MAPPER);
    private static final Executor<List<Author>> AUTHOR_EXECUTOR
            = ExecutorFactory.createQuery(DBConnection.prepareStatement("""
            SELECT id, first_name, last_name
            FROM authors
            WHERE first_name = ? AND last_name = ?"""), AUTHOR_MAPPER);
    private static final Executor<List<Book>> BOOK_EXECUTOR
            = ExecutorFactory.createQuery(DBConnection.prepareStatement("""
            SELECT id, title, publisher, year, pages
            FROM books
            WHERE title = ?"""), BOOK_MAPPER);

    static {
        try (var stmt = DBConnection.createStatement()) {
            String sql = "SELECT id, name FROM subfields";
            SUBFIELDS = Set.copyOf(ExecutorFactory.createQuery(stmt, sql, SUBFIELD_MAPPER).execute());
        } catch (SQLException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    /**
     * Privater Konstruktor; eine Erzeugung einer Klassen-Instanz ist nicht nötig
     */
    private LiteratureQuery() {
    }

    /**
     * Gibt die statisch geladenen {@link Subfield Teilgebiete} zurück
     *
     * @return Alle aus der Datenbank geladenen Teilgebiete
     */
    public static Set<Subfield> getSubfields() {
        return SUBFIELDS;
    }

    /**
     * Lädt alle {@link Book Bücher} aus der Datenbank
     *
     * @return Alle in der Datenbank gespeicherten Bücher
     * @throws SQLException Wenn beim Laden eine solche Ausnahme geworfen wird
     */
    public static Set<Book> queryBooks() throws SQLException {
        return queryBooks(EMPTY_FILTER);
    }

    /**
     * Lädt alle {@link Book Bücher} mittels eines {@link BookFilter Filters} aus der Datenbank
     *
     * @param filter Der Filter, von welchem die geladenen Bücher spezifiziert werden
     * @return Die geladenen Bücher
     * @throws SQLException Wenn beim Laden eine solche Ausnahme geworfen wird
     */
    public static Set<Book> queryBooks(BookFilter filter) throws SQLException {
        final String bookSQL = String.format(
                BOOK_SQL,
                Optional.ofNullable(filter.titleSearch()).orElse(""),
                filter.yearRange().min(),
                filter.yearRange().max(),
                filter.pageRange().min(),
                filter.pageRange().max(),
                filter.ratingRange().min(),
                filter.ratingRange().max(),
                Optional.of(filter)
                        .map(BookFilter::subfields)
                        .filter(s -> !s.isEmpty())
                        .orElse(SUBFIELDS)
                        .stream()
                        .map(Subfield::id)
                        .map(String::valueOf)
                        .collect(Collectors.joining(",")),
                Optional.ofNullable(filter.authorSearch()).orElse("")
        );

        try (var statement = DBConnection.createStatement()) {
            Set<Book> books = new HashSet<>();
            List<Book> books0 = ExecutorFactory.createQuery(statement, bookSQL, BOOK_MAPPER).execute();
            for (Book book : books0) {
                books.add(new Book(
                        book.id(),
                        book.title(),
                        BOOK_AUTHOR_EXECUTOR.execute(book.id()),
                        book.publisher(),
                        book.year(),
                        book.pages(),
                        book.rating(),
                        Set.copyOf(BOOK_SUBFIELD_EXECUTOR.execute(book.id()))
                ));
            }
            return Set.copyOf(books);
        }
    }

    /**
     * Lädt alle {@link Author Autoren}, die einen gegebenen Namen haben,
     * aus der Datenbank, und gibt den ersten gefunden zurück
     *
     * @param firstName Der Vorname des zu suchenden Autors
     * @param lastName  Der Nachname des zu suchenden Autors
     * @return Der erste Autor, dessen Namen dem gesuchten entspricht
     * @throws SQLException Wenn beim Laden eine solche Ausnahme geworfen wird
     */
    static Author queryAuthorByName(String firstName, String lastName) throws SQLException {
        return AUTHOR_EXECUTOR.execute(firstName, lastName).get(0);
    }

    /**
     * Lädt alle {@link Book Bücher}, deren Titel dem gegebenen entspricht, aus der Datenbank,
     * und gibt den ersten gefunden zurück
     *
     * @param title Der zu suchende Buchtitel
     * @return Das erste Buch, dessen Titel dem gesuchten entspricht
     * @throws SQLException Wenn beim Laden eine solche Ausnahme geworfen wird
     */
    static Book queryBookByTitle(String title) throws SQLException {
        return BOOK_EXECUTOR.execute(title).get(0);
    }
}
