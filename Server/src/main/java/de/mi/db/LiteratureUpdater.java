package de.mi.db;

import de.mi.common.Author;
import de.mi.common.Book;
import de.mi.common.Subfield;

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
        UpdateStatements.DELETE_BOOK.executor.execute(bookID);
        UpdateStatements.DELETE_BOOK_AUTHORS.executor.execute(bookID);
        UpdateStatements.DELETE_BOOK_SUBFIELDS.executor.execute(bookID);
        UpdateStatements.DELETE_AUTHORS.executor.execute();
    }

    public static void insertBook(Book book) throws SQLException, IllegalArgumentException {
        if (LiteratureQuery.queryBooks().contains(book))
            throw new IllegalArgumentException("book does already exist and thus can not be inserted");

        replaceBook(book);
    }

    private static void replaceBook(Book book) throws SQLException {
        var values = new Object[]{book.id(), book.title(), book.publisher(), book.year(), book.pages()};
        UpdateStatements.REPLACE_BOOK.executor.execute(values);

        for (Author a : book.authors()) {
            UpdateStatements.REPLACE_AUTHOR.executor.execute(a.id(), a.firstName(), a.lastName());
            UpdateStatements.INSERT_BOOK_AUTHORS.executor.execute(a.id(), book.id());
        }

        for (Subfield s : book.subfields()) {
            UpdateStatements.INSERT_BOOK_SUBFIELDS.executor.execute(book.id(), s.id());
        }
    }
}
