package de.mi.server;

import de.mi.common.Author;
import de.mi.common.Book;
import de.mi.common.Subfield;
import de.mi.server.sql.Executor;
import de.mi.server.sql.ExecutorFactory;

import java.sql.SQLException;

public final class LiteratureUpdater {

    private LiteratureUpdater() {
    }

    public static void updateBook(Book book) throws SQLException, IllegalArgumentException {
        var old = LiteratureQuery.queryBooks().stream().filter(b -> b.equals(book)).findFirst();
        if (old.isEmpty())
            throw new IllegalArgumentException("book does not exist and thus can not be updated");

        var values = new Object[]{book.title(), book.publisher(), book.year(), book.pages(), book.rating(), book.id()};
        Queries.UPDATE_BOOK.execute(values);

        for (Author author : book.authors()) {
            if (author.id() == 0) insertAuthor(book, author);
            else {
                Queries.UPDATE_AUTHOR.execute(author.firstName(), author.lastName(), author.id());
                Queries.INSERT_BOOK_AUTHORS.execute(book.id(), author.id());
            }
        }

        for (Subfield subfield : book.subfields()) {
            Queries.INSERT_BOOK_SUBFIELDS.execute(book.id(), subfield.id());
        }

        for (Author author : old.get().authors()) {
            if (!book.authors().contains(author)) {
                Queries.DELETE_BOOK_AUTHOR.execute(book.id(), author.id());
            }
        }

        for (Subfield subfield : old.get().subfields()) {
            if (!book.subfields().contains(subfield))
                Queries.DELETE_BOOK_SUBFIELD.execute(book.id(), subfield.id());
        }
    }

    public static void deleteBook(int bookID) throws SQLException {
        Queries.DELETE_BOOK.execute(bookID);
        Queries.DELETE_BOOK_AUTHORS_ALL.execute(bookID);
        Queries.DELETE_BOOK_SUBFIELDS_ALL.execute(bookID);
        Queries.DELETE_AUTHORS.execute();
    }

    public static void insertBook(Book book) throws SQLException, IllegalArgumentException {
        if (book.id() != 0 || LiteratureQuery.queryBooks().contains(book))
            throw new IllegalArgumentException("book does already exist and thus can not be inserted");

        var values = new Object[]{book.title(), book.publisher(), book.year(), book.pages(), book.rating()};
        Queries.INSERT_BOOK.execute(values);
        Book inserted = LiteratureQuery.queryBookByTitle(book.title());

        for (Author author : book.authors()) {
            if (author.id() == 0) insertAuthor(inserted, author);
            else Queries.INSERT_BOOK_AUTHORS.execute(inserted.id(), author.id());
        }

        for (Subfield subfield : book.subfields()) {
            Queries.INSERT_BOOK_SUBFIELDS.execute(inserted.id(), subfield.id());
        }
    }

    private static void insertAuthor(Book book, Author author) throws SQLException {
        Queries.INSERT_AUTHOR.execute(author.firstName(), author.lastName());
        Author inserted = LiteratureQuery.queryAuthorByName(author.firstName(), author.lastName());
        if (inserted == null) throw new IllegalStateException("unable to find inserted author: " + author);
        Queries.INSERT_BOOK_AUTHORS.execute(book.id(), inserted.id());
    }

    @SuppressWarnings({"java:S2972", "java:S1135"}) // TODO: remove suppression
    enum Queries implements Executor<Integer> {
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
                INSERT INTO books (title, publisher, year, pages, rating)
                VALUES (?, ?, ?, ?, ?)"""),
        INSERT_AUTHOR("""
                INSERT INTO authors (first_name, last_name)
                VALUES (?, ?)"""),
        UPDATE_BOOK("""
                UPDATE books
                SET title = ?, publisher = ?, year = ?, pages = ?, rating = ?
                WHERE id = ?"""),
        UPDATE_AUTHOR("""
                UPDATE authors
                SET first_name = ?, last_name = ?
                WHERE id = ?""");

        private final Executor<Integer> executor;

        Queries(String sql) {
            executor = ExecutorFactory.createUpdater(DBConnection.prepareStatement(sql));
        }

        @Override
        public Integer execute(Object... values) throws SQLException {
            return executor.execute(values);
        }
    }
}
