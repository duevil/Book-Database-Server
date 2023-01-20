package de.mi.other;

import de.mi.common.Subfield;
import de.mi.server.LiteratureQuery;
import de.mi.server.sql.SQLExceptionHandler;

import java.io.IOException;
import java.io.PrintStream;
import java.io.UncheckedIOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.sql.SQLException;
import java.util.Map;
import java.util.StringJoiner;
import java.util.logging.Logger;

public class BookReader {
    private static final String NL = System.lineSeparator();

    public static void main(String[] args) {
        try (var outStream = Files.newOutputStream(Path.of("literature.sql"));
             var file = new PrintStream(outStream, true, Charset.defaultCharset())) {
            file.println("""
                    # noinspection SpellCheckingInspectionForFile
                    # (C) Malte Kasolowsky 2023
                                    
                    USE informatik;
                                        
                    # ------ table books ------
                                        
                    DROP TABLE IF EXISTS books;
                    CREATE TABLE books
                    (
                        id        SMALLINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
                        title     VARCHAR(128) NOT NULL,
                        publisher VARCHAR(128) NOT NULL,
                        year      SMALLINT     NOT NULL,
                        pages     SMALLINT     NOT NULL,
                        rating    SMALLINT     NOT NULL
                    );
                                        
                    # ------ table subfields ------
                                        
                    DROP TABLE IF EXISTS subfields;
                    CREATE TABLE subfields
                    (
                        id   SMALLINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
                        name varchar(64) NOT NULL
                    );
                                        
                    # ------ table authors ------
                                        
                    DROP TABLE IF EXISTS authors;
                    CREATE TABLE authors
                    (
                        id         SMALLINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
                        first_name VARCHAR(64) NOT NULL,
                        last_name  VARCHAR(64) NOT NULL,
                        UNIQUE (first_name, last_name)
                    );
                                        
                    # ------ table book authors ------
                                        
                    DROP TABLE IF EXISTS book_authors;
                    CREATE TABLE book_authors
                    (
                        author_id SMALLINT REFERENCES authors (id),
                        book_id   SMALLINT REFERENCES books (id),
                        PRIMARY KEY (author_id, book_id)
                    );
                                        
                    # ------ table book subfields ------
                                        
                    DROP TABLE IF EXISTS book_subfields;
                    CREATE TABLE book_subfields
                    (
                        book_id     SMALLINT REFERENCES books (id),
                        subfield_id SMALLINT REFERENCES subfields (id),
                        PRIMARY KEY (book_id, subfield_id)
                    );
                    """);

            file.printf("%n# ------ data subfields ------ %n%n");

            var subfields = LiteratureQuery.getSubfields();
            for (Subfield subfield : subfields) {
                var subfieldValues = Map.of(
                        "id", String.valueOf(subfield.id()),
                        "name", quoteIfy(subfield.name())
                );
                file.println(insert("subfields", subfieldValues));
            }

            file.printf("%n# ------ data books ------ %n%n");

            var books = LiteratureQuery.queryBooks();
            int i = 0;
            for (var book : books) {

                file.printf("%n# ---- data book nr. %03d ---- %n%n", ++i);

                var bookValues = Map.of(
                        "id", String.valueOf(book.id()),
                        "title", quoteIfy(book.title()),
                        "publisher", quoteIfy(book.publisher()),
                        "year", String.valueOf(book.year()),
                        "pages", String.valueOf(book.pages()),
                        "rating", String.valueOf(book.rating())
                );
                file.println(insert("books", bookValues));

                file.printf("# ---- data book nr. %03d authors ---- %n%n", i);

                int j = 0;
                for (var author : book.authors()) {

                    file.printf("# -- data author nr. %03d -- %n", ++j);

                    var authorValues = Map.of(
                            "id", String.valueOf(author.id()),
                            "first_name", quoteIfy(author.firstName()),
                            "last_name", quoteIfy(author.lastName())
                    );
                    file.println(insert("authors", authorValues));

                    file.printf("# -- data book author nr. %03d -- %n", j);

                    var bookAuthorValues = Map.of(
                            "book_id", String.valueOf(book.id()),
                            "author_id", String.valueOf(author.id())
                    );

                    file.println(insert("book_authors", bookAuthorValues));

                }

                file.printf("# ---- data book nr. %03d subfields ---- %n%n", i);

                j = 0;
                for (var subfield : book.subfields()) {

                    file.printf("# -- book subfield nr. %03d -- %n", ++j);

                    var bookSubfieldValues = Map.of(
                            "book_id", String.valueOf(book.id()),
                            "subfield_id", String.valueOf(subfield.id())
                    );
                    file.println(insert("book_subfields", bookSubfieldValues));
                }
            }
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        } catch (SQLException e) {
            SQLExceptionHandler.handle(e, Logger.getAnonymousLogger());
        }
    }

    private static String insert(String table, Map<String, String> values) {
        final var sj0 = new StringJoiner(", ", "INSERT INTO " + table + " (", ")" + NL);
        final var sj1 = new StringJoiner(", ", "VALUES (", ");" + NL);
        values.keySet().forEach(sj0::add);
        values.values().forEach(sj1::add);
        return "%s%s".formatted(sj0.toString(), sj1.toString());
    }

    private static String quoteIfy(String s) {
        return "'%s'".formatted(s.replace("'", "''"));
    }
}
