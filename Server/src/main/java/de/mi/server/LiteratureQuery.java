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
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

public final class LiteratureQuery {
    private static final BookFilter EMPTY_FILTER = BookFilter.builder().build();
    private static final Set<Subfield> SUBFIELDS = new HashSet<>();
    private static final String BOOK_SQL = """
              SELECT title, publisher, year, pages, rating
              FROM books
              WHERE LOWER(title) LIKE '%%%s%%'
                        AND year BETWEEN %d AND %d
                        AND pages BETWEEN %d AND %d
                        AND rating BETWEEN %d AND %d
                AND EXISTS(SELECT name
                           FROM subfields
                                    JOIN book_subfields ON name = subfield
                           WHERE book = title
                             AND name IN (%s))
                AND EXISTS(SELECT first_name, last_name
                           FROM authors
                                    JOIN book_authors ON first_name = author_first_name
                               AND last_name = author_last_name
                           WHERE book = title
                             AND LOWER(CONCAT_WS(' ', first_name, last_name)) LIKE '%%%s%%')""";
    private static final SQLQueryExecutor<Author> BOOK_AUTHOR_EXECUTOR
            = new SQLQueryExecutor<>(DBConnection.prepareStatement("""
            SELECT first_name, last_name
            FROM authors
                     JOIN book_authors ON first_name = author_first_name
                AND last_name = author_last_name
            WHERE book = ?"""), LiteratureMapper.AUTHOR_MAPPER);
    private static final SQLQueryExecutor<Author> AUTHOR_EXECUTOR
            = new SQLQueryExecutor<>(DBConnection.prepareStatement("""
            SELECT first_name, last_name
            FROM authors
                     JOIN book_authors ON first_name = author_first_name
                AND last_name = author_last_name"""), LiteratureMapper.AUTHOR_MAPPER);
    private static final SQLQueryExecutor<Subfield> BOOK_SUBFIELD_EXECUTOR
            = new SQLQueryExecutor<>(DBConnection.prepareStatement("""
            SELECT name
            FROM subfields JOIN book_subfields ON name = subfield
            WHERE book = ?"""), LiteratureMapper.SUBFIELD_MAPPER);

    static {
        try (var stmt = DBConnection.createStatement()) {
            var sql = "SELECT name FROM subfields";
            SUBFIELDS.addAll(new SQLQueryExecutor<>(stmt, sql, LiteratureMapper.SUBFIELD_MAPPER).execute());
        } catch (SQLException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    private LiteratureQuery() {
    }

    public static Set<Subfield> querySubfields() {
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
                filter.yearRange().min(), filter.yearRange().max(),
                filter.pageRange().min(), filter.pageRange().max(),
                1, 5,
                Optional.of(filter)
                        .map(BookFilter::subfields)
                        .filter(s -> !s.isEmpty())
                        .map(Collection::stream)
                        .orElseGet(SUBFIELDS::stream)
                        .map(Subfield::name)
                        .map(s -> s.replace("'", "''"))
                        .collect(Collectors.joining("','", "'", "'")),
                filter.authorSearch().orElse("")
        );

        try (var stmt = DBConnection.createStatement()) {
            SQLQueryExecutor<Book> query = new SQLQueryExecutor<>(stmt, bookSQL, LiteratureMapper.BOOK_MAPPER);
            for (Book b : query.execute()) {
                books.add(new Book(
                        b.title(),
                        Set.copyOf(BOOK_AUTHOR_EXECUTOR.execute(b.title())),
                        b.publisher(),
                        b.year(),
                        b.pages(),
                        b.rating(),
                        Set.copyOf(BOOK_SUBFIELD_EXECUTOR.execute(b.title()))
                ));
            }
        }

        return Collections.unmodifiableSet(books);
    }

    public static List<Author> queryAuthors(String book) throws SQLException {
        return book == null
                ? AUTHOR_EXECUTOR.execute()
                : BOOK_AUTHOR_EXECUTOR.execute(book);
    }
}
