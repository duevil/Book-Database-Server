package de.mi.db;

import de.mi.common.Author;
import de.mi.common.Book;
import de.mi.common.BookFilter;
import de.mi.common.BookFilterBuilder;
import de.mi.common.Subfield;
import de.mi.mapper.LiteratureMapper;
import de.mi.sql.SQLExceptionHandler;
import de.mi.sql.SQLExecutorFactory;
import de.mi.sql.SQLQueryExecutor;

import java.sql.SQLException;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;

public final class LiteratureQuery {
    private static final BookFilter EMPTY_FILTER = new BookFilterBuilder().build();
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
    private static final String BOOK_AUTHOR_SQL = """
            SELECT id, first_name, last_name
            FROM authors JOIN book_authors ON id = author_id
            WHERE book_id = ?""";
    private static final String BOOK_SUBFIELD_SQL = """
            SELECT id, name
            FROM subfields JOIN book_subfields ON id = subfield_id
            WHERE book_id = ?""";
    private static final SQLExecutorFactory<SQLQueryExecutor<Subfield>> SUBFIELD_QUERY_FACTORY;
    private static final SQLExecutorFactory<SQLQueryExecutor<Book>> BOOK_QUERY_FACTORY;
    private static final SQLQueryExecutor<Subfield> BOOK_SUBFIELD_EXECUTOR;
    private static final SQLQueryExecutor<Author> BOOK_AUTHOR_EXECUTOR;

    static {
        SQLQueryExecutor<Subfield> bookSubfieldExecutor = null;
        SQLQueryExecutor<Author> bookAuthorExecutor = null;

        SUBFIELD_QUERY_FACTORY = SQLExecutorFactory.createQuery(LiteratureMapper.SUBFIELD_MAPPER);
        BOOK_QUERY_FACTORY = SQLExecutorFactory.createQuery(LiteratureMapper.BOOK_MAPPER);

        try {
            SUBFIELD_QUERY_FACTORY.setStatement(DBConnection.createStatement());
            BOOK_QUERY_FACTORY.setStatement(DBConnection.createStatement());

            var subfieldSQL = "SELECT id, name FROM subfields";
            var subfields = SUBFIELD_QUERY_FACTORY.setSqlString(subfieldSQL).get().execute();
            SUBFIELDS.addAll(subfields);

            var bookSubfieldStatement = DBConnection.prepareStatement(BOOK_SUBFIELD_SQL);
            var bookAuthorStatement = DBConnection.prepareStatement(BOOK_AUTHOR_SQL);
            bookSubfieldExecutor = SQLExecutorFactory.createQuery(LiteratureMapper.SUBFIELD_MAPPER)
                    .setStatement(bookSubfieldStatement)
                    .get();
            bookAuthorExecutor = SQLExecutorFactory.createQuery(LiteratureMapper.AUTHOR_MAPPER)
                    .setStatement(bookAuthorStatement)
                    .get();
        } catch (SQLException e) {
            SQLExceptionHandler.handle(e);
        }

        BOOK_SUBFIELD_EXECUTOR = bookSubfieldExecutor;
        BOOK_AUTHOR_EXECUTOR = bookAuthorExecutor;
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
        final Set<Book> books = new HashSet<>();
        var bookList = BOOK_QUERY_FACTORY.setSqlString(bookSQL).get().execute();

        for (Book b : bookList) {
            var authors = BOOK_AUTHOR_EXECUTOR.execute(b.id());
            var subfields = BOOK_SUBFIELD_EXECUTOR.execute(b.id());
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
