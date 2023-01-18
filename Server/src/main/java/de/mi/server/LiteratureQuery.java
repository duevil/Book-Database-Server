package de.mi.server;

import de.mi.common.Author;
import de.mi.common.Book;
import de.mi.common.BookFilter;
import de.mi.common.Subfield;
import de.mi.server.mapper.LiteratureMapper;
import de.mi.server.sql.SQLExecutor;
import de.mi.server.sql.SQLExecutorFactory;

import java.sql.SQLException;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

public final class LiteratureQuery {
    private static final BookFilter EMPTY_FILTER = BookFilter.builder().build();
    private static final Set<Subfield> SUBFIELDS = new HashSet<>();
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
    private static final SQLExecutor<List<Author>> BOOK_AUTHOR_EXECUTOR
            = SQLExecutorFactory.createQuery(DBConnection.prepareStatement("""
            SELECT id, first_name, last_name
            FROM authors JOIN book_authors ON id = author_id
            WHERE book_id = ?"""), LiteratureMapper.AUTHOR_MAPPER);
    private static final SQLExecutor<List<Subfield>> BOOK_SUBFIELD_EXECUTOR
            = SQLExecutorFactory.createQuery(DBConnection.prepareStatement("""
            SELECT id, name
            FROM subfields JOIN book_subfields ON id = subfield_id
            WHERE book_id = ?"""), LiteratureMapper.SUBFIELD_MAPPER);
    private static final SQLExecutor<List<Author>> AUTHOR_EXECUTOR
            = SQLExecutorFactory.createQuery(DBConnection.prepareStatement("""
            SELECT id, first_name, last_name
            FROM authors
            WHERE first_name = ? AND last_name = ?"""), LiteratureMapper.AUTHOR_MAPPER);
    private static final SQLExecutor<List<Book>> BOOK_EXECUTOR
            = SQLExecutorFactory.createQuery(DBConnection.prepareStatement("""
            SELECT id, title, publisher, year, pages
            FROM books
            WHERE title = ?"""), LiteratureMapper.BOOK_MAPPER);

    static {
        try (var stmt = DBConnection.createStatement()) {
            var sql = "SELECT id, name FROM subfields";
            SUBFIELDS.addAll(SQLExecutorFactory.createQuery(stmt, sql, LiteratureMapper.SUBFIELD_MAPPER).execute());
        } catch (SQLException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    private LiteratureQuery() {
    }

    public static Set<Subfield> getSubfields() {
        return Set.copyOf(SUBFIELDS);
    }

    public static Set<Book> queryBooks() throws SQLException {
        return queryBooks(EMPTY_FILTER);
    }

    public static Set<Book> queryBooks(BookFilter filter) throws SQLException {
        final Set<Book> books = new HashSet<>();
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
            for (Book b : SQLExecutorFactory.createQuery(statement, bookSQL, LiteratureMapper.BOOK_MAPPER).execute()) {
                books.add(new Book(
                        b.id(),
                        b.title(),
                        BOOK_AUTHOR_EXECUTOR.execute(b.id()),
                        b.publisher(),
                        b.year(),
                        b.pages(),
                        b.rating(),
                        Set.copyOf(BOOK_SUBFIELD_EXECUTOR.execute(b.id()))
                ));
            }
        }

        return Set.copyOf(books);
    }

    static Author queryAuthorByName(String firstName, String lastName) throws SQLException {
        return AUTHOR_EXECUTOR.execute(firstName, lastName).get(0);
    }

    static Book queryBookByTitle(String title) throws SQLException {
        return BOOK_EXECUTOR.execute(title).get(0);
    }
}
