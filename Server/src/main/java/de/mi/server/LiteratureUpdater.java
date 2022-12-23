package de.mi.server;

import de.mi.common.Author;
import de.mi.common.Book;
import de.mi.common.Subfield;
import de.mi.server.sql.SQLExecutorFactory;
import de.mi.server.sql.SQLUpdateExecutor;

import java.sql.SQLException;

public final class LiteratureUpdater {

    private LiteratureUpdater() {
    }

    public static void updateBook(Book book) throws SQLException, IllegalArgumentException {
        if (!LiteratureQuery.queryBooks().contains(book))
            throw new IllegalArgumentException("book does not exist and thus can not be updated");

        replaceBook(book);
    }

    public static void deleteBook(int bookID) throws SQLException {
        Queries.DELETE_BOOK.get().execute(bookID);
        Queries.DELETE_BOOK_AUTHORS.get().execute(bookID);
        Queries.DELETE_BOOK_SUBFIELDS.get().execute(bookID);
        Queries.DELETE_AUTHORS.get().execute();
    }

    public static void insertBook(Book book) throws SQLException, IllegalArgumentException {
        if (LiteratureQuery.queryBooks().contains(book))
            throw new IllegalArgumentException("book does already exist and thus can not be inserted");

        replaceBook(book);
    }

    private static void replaceBook(Book book) throws SQLException {
        var values = new Object[]{book.id(), book.title(), book.publisher(), book.year(), book.pages()};
        Queries.REPLACE_BOOK.get().execute(values);

        for (Author a : book.authors()) {
            Queries.REPLACE_AUTHOR.get().execute(a.id(), a.firstName(), a.lastName());
            Queries.INSERT_BOOK_AUTHORS.get().execute(a.id(), book.id());
        }

        for (Subfield s : book.subfields()) {
            Queries.INSERT_BOOK_SUBFIELDS.get().execute(book.id(), s.id());
        }
    }

    enum Queries {
        DELETE_AUTHORS("DELETE FROM authors WHERE NOT EXISTS(SELECT * FROM book_authors WHERE author_id = id)"),
        DELETE_BOOK("DELETE FROM books WHERE id = ?"),
        DELETE_BOOK_AUTHORS("DELETE FROM book_authors WHERE book_id = ?"),
        DELETE_BOOK_SUBFIELDS("DELETE FROM book_subfields WHERE book_id = ?"),
        INSERT_BOOK_AUTHORS("INSERT INTO book_authors VALUES (?, ?) ON DUPLICATE KEY UPDATE book_id = book_id"),
        INSERT_BOOK_SUBFIELDS("INSERT INTO book_subfields VALUES (?, ?) ON DUPLICATE KEY UPDATE book_id = book_id"),
        REPLACE_AUTHOR("REPLACE INTO authors (id, first_name, last_name) VALUES (?, ?, ?)"),
        REPLACE_BOOK("REPLACE INTO books (id, title, publisher, year, pages) VALUES (?, ?, ?, ?, ?)");

        private final SQLUpdateExecutor executor;

        Queries(String sql) {
            executor = SQLExecutorFactory.createUpdater(DBConnection.prepareStatement(sql));
        }

        public SQLUpdateExecutor get() {
            return executor;
        }
    }
}
