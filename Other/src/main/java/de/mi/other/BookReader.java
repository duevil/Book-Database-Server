package de.mi.other;

import de.mi.server.LiteratureQuery;

import java.io.PrintStream;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;
import java.util.StringJoiner;

public class BookReader {
    private static final String NL = System.lineSeparator();

    public static void main(String[] args) throws Exception {
        try (var outStream = Files.newOutputStream(Path.of("book.sql"));
             var file = new PrintStream(outStream, true, Charset.defaultCharset())) {
            file.println("""
                    # noinspection SpellCheckingInspectionForFile
                    # (C) Malte Kasolowsky 2023
                    
                    USE informatik;
                                        
                    # ------ table books ------
                                        
                    DROP TABLE IF EXISTS books;
                    CREATE TABLE books(
                        title VARCHAR(128) PRIMARY KEY,
                        publisher VARCHAR(128),
                        year SMALLINT UNSIGNED,
                        pages SMALLINT UNSIGNED,
                        rating SMALLINT UNSIGNED DEFAULT 3
                    );
                                        
                    # ------ table authors ------
                                        
                    DROP TABLE IF EXISTS authors;
                    CREATE TABLE authors (
                        first_name VARCHAR(64),
                        last_name VARCHAR(64),
                        PRIMARY KEY (first_name, last_name)
                    );
                                        
                    # ------ table subfields ------
                                        
                    DROP TABLE IF EXISTS subfields;
                    CREATE TABLE subfields (
                        name VARCHAR(64) PRIMARY KEY
                    );
                                        
                    # ------ table book autors ------
                                        
                    DROP TABLE IF EXISTS book_authors;
                    CREATE TABLE book_authors (
                        book VARCHAR(128) REFERENCES books(title),
                        author_first_name VARCHAR(64) REFERENCES authors(first_name),
                        author_last_name VARCHAR(64) REFERENCES authors(last_name),
                        PRIMARY KEY (book, author_first_name, author_last_name)
                    );
                                        
                    # ------ table book subfields ------
                                        
                    DROP TABLE IF EXISTS book_subfields;
                    CREATE TABLE book_subfields (
                        book VARCHAR(128) REFERENCES books(title),
                        subfield VARCHAR(64) REFERENCES subfields(name),
                        PRIMARY KEY (book, subfield)
                    );
                    """);

            file.printf("%n# ------ data subfields ------ %n%n");

            for (String name : new String[]{
                    "Artificial intelligence",
                    "Programming languages and logic",
                    "Scientific computing applications",
                    "Theory of computation",
                    "Data structures and algorithms",
                    "Computer architecture and organization",
                    "Computer networks",
                    "Computer security in cryptography",
                    "Databases and data mining",
                    "Computer graphics and visualization",
                    "Image and sound processing",
                    "Concurrent, parallel and distributed computing",
                    "Human-computer interaction",
                    "Software engineering",
                    "Information and coding theory"
            }) {
                var subfieldValues = Map.of("name", quoteIfy(name));
                file.println(insert("subfields", subfieldValues));
            }

            file.printf("%n# ------ data books ------ %n");

            var books = LiteratureQuery.queryBooks();
            int i = 0;
            for (var book : books) {

                file.printf("%n# ---- data book nr. %03d ---- %n", ++i);

                var bookValues = Map.of(
                        "title", quoteIfy(book.title()),
                        "publisher", quoteIfy(book.publisher()),
                        "year", String.valueOf(book.year()),
                        "pages", String.valueOf(book.pages()),
                        "rating", String.valueOf(book.rating())
                );
                file.println(insert("books", bookValues));

                file.printf("%n# ---- data book authors ---- %n");

                int j = 0;
                for (var author : book.authors()) {

                    file.printf("%n# -- data author nr. %03d -- %n", ++j);

                    var authorValues = Map.of(
                            "first_name", quoteIfy(author.firstName()),
                            "last_name", quoteIfy(author.lastName())
                    );
                    file.println(insert("authors", authorValues));

                    file.printf("# -- data book author nr. %03d -- %n", j);

                    var bookAuthorValues = Map.of(
                            "book", quoteIfy(book.title()),
                            "author_first_name", quoteIfy(author.firstName()),
                            "author_last_name", quoteIfy(author.lastName())
                    );

                    file.println(insert("book_authors", bookAuthorValues));

                }

                file.printf("%n# ---- data book subfields ---- %n");

                j = 0;
                for (var subfield : book.subfields()) {

                    file.printf("%n# -- book subfield nr. %03d -- %n", ++j);

                    var bookSubfieldValues = Map.of(
                            "book", quoteIfy(book.title()),
                            "subfield", quoteIfy(subfield.name())
                    );
                    file.println(insert("book_subfields", bookSubfieldValues));
                }
            }
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
