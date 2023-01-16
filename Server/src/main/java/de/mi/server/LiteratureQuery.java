package de.mi.server;

import de.mi.common.Author;
import de.mi.common.Book;
import de.mi.common.BookFilter;
import de.mi.common.Subfield;
import de.mi.server.mapper.LiteratureMapper;
import de.mi.server.sql.SQLQueryExecutor;

import java.sql.SQLException;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

public final class LiteratureQuery {
    private static final BookFilter EMPTY_FILTER = BookFilter.builder().build();
    private static final Set<Subfield> SUBFIELDS = new HashSet<>();
    private static final String BOOK_SQL = """
            SELECT id, title, publisher, year, pages
            FROM books
            WHERE LOWER(title) LIKE '%%%s%%'
              AND year BETWEEN %d AND %d
              AND pages BETWEEN %d AND %d
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
    private static final SQLQueryExecutor<Author> BOOK_AUTHOR_EXECUTOR = new SQLQueryExecutor<>(DBConnection.prepareStatement("""
            SELECT id, first_name, last_name
            FROM authors JOIN book_authors ON id = author_id
            WHERE book_id = ?"""), LiteratureMapper.AUTHOR_MAPPER);
    private static final SQLQueryExecutor<Subfield> BOOK_SUBFIELD_EXECUTOR = new SQLQueryExecutor<>(DBConnection.prepareStatement("""
            SELECT id, name
            FROM subfields JOIN book_subfields ON id = subfield_id
            WHERE book_id = ?"""), LiteratureMapper.SUBFIELD_MAPPER);
    private static final SQLQueryExecutor<Author> AUTHOR_EXECUTOR = new SQLQueryExecutor<>(DBConnection.prepareStatement("""
            SELECT id, first_name, last_name
            FROM authors
            WHERE first_name = ? AND last_name = ?"""), LiteratureMapper.AUTHOR_MAPPER);
    private static final SQLQueryExecutor<Book> BOOK_EXECUTOR = new SQLQueryExecutor<>(DBConnection.prepareStatement("""
            SELECT id, title, publisher, year, pages
            FROM books
            WHERE title = ?"""), LiteratureMapper.BOOK_MAPPER);

    static {
        try (var stmt = DBConnection.createStatement()) {
            var sql = "SELECT id, name FROM subfields";
            SUBFIELDS.addAll(new SQLQueryExecutor<>(stmt, sql, LiteratureMapper.SUBFIELD_MAPPER).execute());
        } catch (SQLException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    private LiteratureQuery() {
    }

    public static Set<Subfield> getSubfields() {
        return Collections.unmodifiableSet(SUBFIELDS);
    }

    public static Set<Book> queryBooks() throws SQLException {
        return queryBooks(EMPTY_FILTER);
    }

    public static Set<Book> queryBooks(BookFilter filter) throws SQLException {
        final Set<Book> books = new HashSet<>();
        final String bookSQL = String.format(
                BOOK_SQL,
                filter.titleSearch().orElse(""),
                filter.yearRange().min(),
                filter.yearRange().max(),
                filter.pageRange().min(),
                filter.pageRange().max(),
                Optional.of(filter)
                        .map(BookFilter::subfields)
                        .filter(s -> !s.isEmpty())
                        .map(Collection::stream)
                        .orElseGet(SUBFIELDS::stream)
                        .map(Subfield::id)
                        .map(String::valueOf)
                        .collect(Collectors.joining(",")),
                filter.authorSearch().orElse("")
        );

        try (var stmt = DBConnection.createStatement()) {
            SQLQueryExecutor<Book> query = new SQLQueryExecutor<>(stmt, bookSQL, LiteratureMapper.BOOK_MAPPER);
            for (Book b : query.execute()) {
                books.add(new Book(
                        b.id(),
                        b.title(),
                        BOOK_AUTHOR_EXECUTOR.execute(b.id()),
                        b.publisher(),
                        b.year(),
                        b.pages(),
                        Set.copyOf(BOOK_SUBFIELD_EXECUTOR.execute(b.id()))
                ));
            }
        }

        return Collections.unmodifiableSet(books);
    }

    static Author queryAuthorByName(String firstName, String lastName) throws SQLException {
        return AUTHOR_EXECUTOR.execute(firstName, lastName).get(0);
    }

    static Book queryBookByTitle(String title) throws SQLException {
        return BOOK_EXECUTOR.execute(title).get(0);
    }
}
