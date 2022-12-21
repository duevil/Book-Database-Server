package de.mi.db;

import de.mi.common.Author;
import de.mi.common.Book;
import de.mi.common.BookFilter;
import de.mi.common.Subfield;
import de.mi.mapper.LiteratureMapper;
import de.mi.sql.SQLExecutorFactory;
import de.mi.sql.SQLQueryExecutor;

import java.sql.SQLException;
import java.util.Collections;
import java.util.HashSet;
import java.util.Locale;
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
                           AND LOWER(CONCAT_WS(a.first_name, a.last_name)) LIKE '%%%s%%')""";
    private static final SQLQueryExecutor<Author> BOOK_AUTHOR_EXECUTOR = SQLExecutorFactory.createQuery(
            DBConnection.prepareStatement("""
                    SELECT id, first_name, last_name
                    FROM authors JOIN book_authors ON id = author_id
                    WHERE book_id = ?"""),
            LiteratureMapper.AUTHOR_MAPPER
    );
    private static final SQLQueryExecutor<Subfield> BOOK_SUBFIELD_EXECUTOR = SQLExecutorFactory.createQuery(
            DBConnection.prepareStatement("""
                    SELECT id, name
                    FROM subfields JOIN book_subfields ON id = subfield_id
                    WHERE book_id = ?"""),
            LiteratureMapper.SUBFIELD_MAPPER
    );

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
                filterSubfieldsToString(filter),
                filter.authorSearch().orElse("")
        );

        try (var stmt = DBConnection.createStatement()) {
            SQLQueryExecutor<Book> query = SQLExecutorFactory.createQuery(stmt, bookSQL, LiteratureMapper.BOOK_MAPPER);
            for (Book b : query.execute()) {
                books.add(new Book(
                        b.id(),
                        b.title(),
                        Set.copyOf(BOOK_AUTHOR_EXECUTOR.execute(b.id())),
                        b.publisher(),
                        b.year(),
                        b.pages(),
                        Set.copyOf(BOOK_SUBFIELD_EXECUTOR.execute(b.id()))
                ));
            }
        }

        return Collections.unmodifiableSet(books);
    }

    private static String filterSubfieldsToString(BookFilter filter) {
        var idStream = filter.subfieldIDs().isEmpty()
                ? SUBFIELDS.stream().map(Subfield::id)
                : filter.subfieldIDs().stream();
        return idStream.map(String::valueOf).collect(Collectors.joining(","));
    }

    public static int getNextID(String type) throws SQLException {
        var sql = "SELECT MAX(id) AS max_id FROM " + switch (type.toLowerCase(Locale.ROOT)) {
            case "book" -> "books";
            case "author" -> "authors";
            case "subfield" -> "subfields";
            default -> throw new IllegalArgumentException("invalid class");
        };
        try (var stmt = DBConnection.createStatement()) {
            return SQLExecutorFactory.createQuery(stmt, sql, m -> (Integer) m.get("max_id")).execute().get(0) + 1;
        }
    }
}
