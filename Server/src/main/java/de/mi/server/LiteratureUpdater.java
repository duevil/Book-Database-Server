package de.mi.server;

import de.mi.common.Book;
import de.mi.server.sql.SQLUpdateExecutor;

import java.sql.SQLException;

public final class LiteratureUpdater {

    private LiteratureUpdater() {
    }

    public static void updateBook(Book book) throws SQLException, IllegalArgumentException {
        var old = LiteratureQuery.queryBooks().stream().filter(b -> b.equals(book)).findFirst();
        if (old.isEmpty())
            throw new IllegalArgumentException("book does not exist and thus can not be updated");

        insertOrUpdateBook(book);

        // remove all authors and subfields which were removed in the updated book
        for (var author : old.get().authors()) {
            if (!book.authors().contains(author))
                Queries.DELETE_BOOK_AUTHOR.execute(book.title(), author.firstName(), author.lastName());
        }
        Queries.DELETE_AUTHORS_ALL.execute();
        for (var subfield : old.get().subfields()) {
            if (!book.subfields().contains(subfield))
                Queries.DELETE_BOOK_SUBFIELD.execute(book.title(), subfield.name());
        }
    }

    public static void deleteBook(String bookTitle) throws SQLException {
        if (bookTitle != null) {
            Queries.DELETE_BOOK.execute(bookTitle);
            Queries.DELETE_BOOK_AUTHORS_ALL.execute(bookTitle);
            Queries.DELETE_BOOK_SUBFIELDS_ALL.execute(bookTitle);
            Queries.DELETE_AUTHORS_ALL.execute();
        }
    }

    public static void insertBook(Book book) throws SQLException, IllegalArgumentException {
        if (LiteratureQuery.queryBooks().contains(book))
            throw new IllegalArgumentException("book does already exist and thus can not be inserted");

        insertOrUpdateBook(book);
    }

    private static void insertOrUpdateBook(Book book) throws SQLException {
        Queries.INSERT_OR_UPDATE_BOOK.execute(book.title(), book.publisher(), book.year(), book.pages(), book.rating());
        for (var author : book.authors()) {
            Queries.INSERT_OR_UPDATE_AUTHOR.execute(author.firstName(), author.lastName());
            Queries.INSERT_BOOK_AUTHOR.execute(book.title(), author.firstName(), author.lastName());
        }
        for (var subfield : book.subfields()) {
            Queries.INSERT_BOOK_SUBFIELD.execute(book.title(), subfield.name());
        }
    }

    private enum Queries {
        DELETE_AUTHORS_ALL("""
                DELETE FROM authors
                WHERE NOT EXISTS(
                    SELECT author_first_name, author_last_name
                    FROM book_authors
                    WHERE first_name = author_first_name AND last_name = author_last_name
                )"""),
        DELETE_BOOK("DELETE FROM books WHERE title = ?"),
        DELETE_BOOK_AUTHOR("""
                DELETE FROM book_authors
                WHERE book = ? AND author_first_name = ? AND author_last_name = ?"""),
        DELETE_BOOK_AUTHORS_ALL("DELETE FROM book_authors WHERE book = ?"),
        DELETE_BOOK_SUBFIELD("""
                DELETE FROM book_subfields
                WHERE book = ? AND subfield = ?"""),
        DELETE_BOOK_SUBFIELDS_ALL("DELETE FROM book_subfields WHERE book = ?"),
        INSERT_OR_UPDATE_AUTHOR("""
                INSERT INTO authors (first_name, last_name)
                VALUES (?, ?)
                ON DUPLICATE KEY UPDATE first_name = VALUES(first_name),
                                        last_name = VALUES(last_name)"""),
        INSERT_OR_UPDATE_BOOK("""
                INSERT INTO books (title, publisher, year, pages, rating)
                VALUES (?, ?, ?, ?, ?)
                ON DUPLICATE KEY UPDATE title = VALUES(title),
                                        publisher = VALUES(publisher),
                                        year = VALUES(year),
                                        pages = VALUES(pages),
                                        rating = VALUES(rating)"""),
        INSERT_BOOK_AUTHOR("""
                INSERT INTO book_authors (book, author_first_name, author_last_name)
                VALUES (?, ?, ?)
                ON DUPLICATE KEY UPDATE book = book"""),
        INSERT_BOOK_SUBFIELD("""
                INSERT INTO book_subfields (book, subfield)
                VALUES (?, ?)
                ON DUPLICATE KEY UPDATE book = book""");

        private final SQLUpdateExecutor executor;

        Queries(String sql) {
            executor = new SQLUpdateExecutor(DBConnection.prepareStatement(sql));
        }

        public void execute(Object... values) throws SQLException {
            executor.execute(values);
        }
    }
}
