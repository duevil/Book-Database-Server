package de.mi.db;

import de.mi.sql.SQLExecutorFactory;
import de.mi.sql.SQLUpdateExecutor;

enum UpdateStatements {
    DELETE_AUTHORS("DELETE FROM authors WHERE NOT EXISTS(SELECT * FROM book_authors WHERE author_id = id)"),
    DELETE_BOOK("DELETE FROM books WHERE id = ?"),
    DELETE_BOOK_AUTHORS("DELETE FROM book_authors WHERE book_id = ?"),
    DELETE_BOOK_SUBFIELDS("DELETE FROM book_subfields WHERE book_id = ?"),
    INSERT_BOOK_AUTHORS("INSERT INTO book_authors VALUES (?, ?) ON DUPLICATE KEY UPDATE book_id = book_id"),
    INSERT_BOOK_SUBFIELDS("INSERT INTO book_subfields VALUES (?, ?) ON DUPLICATE KEY UPDATE book_id = book_id"),
    REPLACE_AUTHOR("REPLACE INTO authors (id, first_name, last_name) VALUES (?, ?, ?)"),
    REPLACE_BOOK("REPLACE INTO books (id, title, publisher, year, pages) VALUES (?, ?, ?, ?, ?)");

    public final SQLUpdateExecutor executor;

    UpdateStatements(String sql) {
        var stmt = DBConnection.prepareStatement(sql);
        this.executor = SQLExecutorFactory.createUpdater().setStatement(stmt).get();
    }
}
