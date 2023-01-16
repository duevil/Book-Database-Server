package de.mi.server;

import de.mi.common.Author;
import de.mi.common.Book;
import de.mi.common.Subfield;
import de.mi.server.sql.SQLUpdateExecutor;

import java.sql.SQLException;

public final class LiteratureUpdater {

    private LiteratureUpdater() {
    }

    public static void updateBook(Book book) throws SQLException, IllegalArgumentException {
        var old = LiteratureQuery.queryBooks().stream().filter(b -> b.equals(book)).findFirst();
        if (old.isEmpty())
            throw new IllegalArgumentException("book does not exist and thus can not be updated");

        var values = new Object[]{book.title(), book.publisher(), book.year(), book.pages(), book.id()};
        Queries.UPDATE_BOOK.get().execute(values);

        for (Author author : book.authors()) {
            if (author.id() == 0) insertAuthor(book, author);
            else {
                Queries.UPDATE_AUTHOR.get().execute(author.firstName(), author.lastName(), author.id());
                Queries.INSERT_BOOK_AUTHORS.get().execute(book.id(), author.id());
            }
        }

        for (Subfield subfield : book.subfields()) {
            Queries.INSERT_BOOK_SUBFIELDS.get().execute(book.id(), subfield.id());
        }

        for (Author author : old.get().authors()) {
            if (!book.authors().contains(author)) {
                Queries.DELETE_BOOK_AUTHOR.get().execute(book.id(), author.id());
            }
        }

        for (Subfield subfield : old.get().subfields()) {
            if (!book.subfields().contains(subfield))
                Queries.DELETE_BOOK_SUBFIELD.get().execute(book.id(), subfield.id());
        }
    }

    public static void deleteBook(int bookID) throws SQLException {
        Queries.DELETE_BOOK.get().execute(bookID);
        Queries.DELETE_BOOK_AUTHORS_ALL.get().execute(bookID);
        Queries.DELETE_BOOK_SUBFIELDS_ALL.get().execute(bookID);
        Queries.DELETE_AUTHORS.get().execute();
    }

    public static void insertBook(Book book) throws SQLException, IllegalArgumentException {
        if (book.id() != 0 || LiteratureQuery.queryBooks().contains(book))
            throw new IllegalArgumentException("book does already exist and thus can not be inserted");

        var values = new Object[]{book.title(), book.publisher(), book.year(), book.pages()};
        Queries.INSERT_BOOK.get().execute(values);
        Book inserted = LiteratureQuery.queryBookByTitle(book.title());

        for (Author author : book.authors()) {
            if (author.id() == 0) insertAuthor(inserted, author);
            else Queries.INSERT_BOOK_AUTHORS.get().execute(inserted.id(), author.id());
        }

        for (Subfield subfield : book.subfields()) {
            Queries.INSERT_BOOK_SUBFIELDS.get().execute(inserted.id(), subfield.id());
        }
    }

    private static void insertAuthor(Book book, Author author) throws SQLException {
        Queries.INSERT_AUTHOR.get().execute(author.firstName(), author.lastName());
        Author inserted = LiteratureQuery.queryAuthorByName(author.firstName(), author.lastName());
        if (inserted == null) throw new IllegalStateException("unable to find inserted author: " + author);
        Queries.INSERT_BOOK_AUTHORS.get().execute(book.id(), inserted.id());
    }

    enum Queries {
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
                INSERT INTO books (title, publisher, year, pages)
                VALUES (?, ?, ?, ?)"""),
        INSERT_AUTHOR("""
                INSERT INTO authors (first_name, last_name)
                VALUES (?, ?)"""),
        UPDATE_BOOK("""
                UPDATE books
                SET title = ?, publisher = ?, year = ?, pages = ?
                WHERE id = ?"""),
        UPDATE_AUTHOR("""
                UPDATE authors
                SET first_name = ?, last_name = ?
                WHERE id = ?""");

        private final SQLUpdateExecutor executor;

        Queries(String sql) {
            executor = new SQLUpdateExecutor(DBConnection.prepareStatement(sql));
        }

        public SQLUpdateExecutor get() {
            return executor;
        }
    }
}
