package de.mi.sql;

import de.mi.common.Book;
import de.mi.common.BookFilter;
import de.mi.common.BookFilterBuilder;
import de.mi.common.Subfield;
import de.mi.mapper.LiteratureMapper;
import de.mi.server.DBConnection;

import java.sql.SQLException;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;

public final class LiteratureQuery {
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
    private static final String BOOK_AUTHOR_SQL = """
            SELECT id, first_name, last_name
            FROM authors JOIN book_authors ON id = author_id
            WHERE book_id = ?""";
    private static final String BOOK_SUBFIELD_SQL = """
            SELECT id, name
            FROM subfields JOIN book_subfields ON id = subfield_id
            WHERE book_id = ?""";
    private static final SQLQueryExecutor BOOK_SUBFIELD_EXECUTOR;
    private static final SQLQueryExecutor BOOK_AUTHOR_EXECUTOR;
    private static final BookFilter EMPTY_FILTER = new BookFilterBuilder().build();
    private static final Set<Subfield> SUBFIELDS = new HashSet<>();

    static {
        final var con = DBConnection.get().connection();
        SQLQueryExecutor bookSubfieldExecutor = null;
        SQLQueryExecutor bookAuthorExecutor = null;
        try {
            var bookSubfieldStatement = con.prepareStatement(BOOK_SUBFIELD_SQL);
            var bookAuthorStatement = con.prepareStatement(BOOK_AUTHOR_SQL);
            bookSubfieldExecutor = SQLQueryExecutor.forPreparedStatement(bookSubfieldStatement);
            bookAuthorExecutor = SQLQueryExecutor.forPreparedStatement(bookAuthorStatement);
        } catch (SQLException e) {
            SQLExceptionHandler.handle(e);
        }
        BOOK_SUBFIELD_EXECUTOR = bookSubfieldExecutor;
        BOOK_AUTHOR_EXECUTOR = bookAuthorExecutor;
    }

    static {
        try {
            var con = DBConnection.get().connection();
            var stmt = con.prepareStatement("SELECT id, name FROM subfields");
            var queryExecutor = SQLQueryExecutor.forPreparedStatement(stmt);
            SUBFIELDS.addAll(queryExecutor.getMappedList(LiteratureMapper.SUBFIELD_MAPPER));
        } catch (SQLException e) {
            SQLExceptionHandler.handle(e);
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
        final String bookSQL = String.format(
                BOOK_SQL,
                filter.titleSearch().orElse(""),
                filter.yearRange().min(),
                filter.yearRange().max(),
                filter.pageRange().min(),
                filter.pageRange().max(),
                (filter.subfields().isEmpty() ? SUBFIELDS : filter.subfields()).stream()
                        .map(Subfield::id)
                        .map(String::valueOf)
                        .collect(Collectors.joining(", ")),
                filter.authorSearch().orElse("")
        );
        final SQLQueryExecutor executor = SQLQueryExecutor.forSQLString(bookSQL);
        final Set<Book> books = new HashSet<>();
        for (Book b : executor.getMappedList(LiteratureMapper.BOOK_MAPPER)) {
            BOOK_AUTHOR_EXECUTOR.getPreparedStatement().setInt(1, b.id());
            BOOK_SUBFIELD_EXECUTOR.getPreparedStatement().setInt(1, b.id());
            var authors = BOOK_AUTHOR_EXECUTOR.getMappedList(LiteratureMapper.AUTHOR_MAPPER);
            var subfields = BOOK_SUBFIELD_EXECUTOR.getMappedList(LiteratureMapper.SUBFIELD_MAPPER);
            books.add(new Book(
                    b.id(),
                    b.title(),
                    Set.copyOf(authors),
                    b.publisher(),
                    b.year(),
                    b.pages(),
                    Set.copyOf(subfields)
            ));
        }
        return Collections.unmodifiableSet(books);
    }
}
