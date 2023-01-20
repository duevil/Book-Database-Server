package de.mi.other;

import com.github.javafaker.Faker;
import de.mi.common.Author;
import de.mi.common.Book;
import de.mi.common.Subfield;

import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Random;
import java.util.Scanner;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@SuppressWarnings({"java:S109", "java:S1123", "java:S1133", "java:S1135"}) // TODO: remove suppression
@Deprecated(since = "rating was added")
public final class BookGenerator {
    private static final Random RND = new Random();
    private static final Scanner SCANNER = new Scanner(System.in, Charset.defaultCharset());
    private static final Subfield[] SUBFIELDS = {
            new Subfield(0x1, "Artificial intelligence"),
            new Subfield(0x2, "Programming languages and logic"),
            new Subfield(0x3, "Scientific computing applications"),
            new Subfield(0x4, "Theory of computation"),
            new Subfield(0x5, "Data structures and algorithms"),
            new Subfield(0x6, "Computer architecture and organization"),
            new Subfield(0x7, "Computer networks"),
            new Subfield(0x8, "Computer security in cryptography"),
            new Subfield(0x9, "Databases and data mining"),
            new Subfield(0xA, "Computer graphics and visualization"),
            new Subfield(0xB, "Image and sound processing"),
            new Subfield(0xC, "Concurrent, parallel and distributed computing"),
            new Subfield(0xD, "Human-computer interaction"),
            new Subfield(0xE, "Software engineering"),
            new Subfield(0xF, "Information and coding theory")
    };
    private static final String PRINT_TEMPLATE = """
            --- book #%d ---
            title: %s
            authors: %s
            publisher: %s
            year: %d
            pages: %d
            subfields: %s
                            
            """;
    private static final String BOOK_SQL_TEMPLATE = """
            INSERT INTO books
            VALUES (%d, '%s', '%s', %d, %d);
                            
            %s%s""";
    private static final String AUTHOR_SQL_TEMPLATE = """
            INSERT INTO authors
            VALUES (%d, '%s', '%s');
                            
            INSERT INTO is_author
            VALUES (%d, %d);
                            
            """;
    private static final String SUBFIELD_SQL_TEMPLATE = """
            INSERT INTO has_subfield
            VALUES (%d, %d);
                            
            """;
    private static final Path GENERATED_FILE_PATH = Path.of("sql-scripts", "literature-data.sql");
    private static int authorID;
    private static int bookID;

    private BookGenerator() {
    }

    @SuppressWarnings("java:S2096")
    public static void main(String[] args) throws IOException {
        List<Book> bookList = generateBooks();
        writeToSQLFile(bookList);
        printBookList(bookList);
    }

    private static List<Book> generateBooks() {
        var bookList = new LinkedList<Book>();
        while (true) {
            System.out.println("-- Subfields: ");
            var subfields = new ArrayList<>(List.of(SUBFIELDS));
            int numSubfields = RND.nextInt(1, 5);
            while (subfields.size() > numSubfields) subfields.remove(RND.nextInt(subfields.size()));
            subfields.forEach(System.out::println);
            System.out.print("-- Enter book title (or 0 to exit): ");
            if (SCANNER.hasNextInt() && SCANNER.nextInt() == 0) break;
            String title = SCANNER.nextLine();
            bookList.add(getBook(title, Set.copyOf(subfields)));
        }
        return bookList;
    }

    private static Book getBook(String title, Set<Subfield> subfields) {
        bookID += 1;
        var authors = Stream.generate(() -> {
            var name = Faker.instance().name();
            authorID += 1;
            return new Author(authorID, name.firstName(), name.lastName());
        }).limit(RND.nextInt(1, 7)).toList();
        int year = RND.nextInt(1920, 2023);
        int pages = RND.nextInt(10, 501);
        return new Book(bookID, title, authors, Faker.instance().book().publisher(), year, pages, 1, subfields);
    }

    private static void writeToSQLFile(List<Book> bookList) throws IOException {
        try (var writer = Files.newBufferedWriter(GENERATED_FILE_PATH)) {
            String sql = bookList.stream().map((Book b) -> {
                String authors = b.authors().stream().map(a -> String.format(
                        AUTHOR_SQL_TEMPLATE,
                        a.id(),
                        a.firstName().replace("'", "''"),
                        a.lastName().replace("'", "''"),
                        a.id(),
                        b.id()
                )).collect(Collectors.joining());
                String subfields = b.subfields().stream().map(s -> String.format(
                        SUBFIELD_SQL_TEMPLATE,
                        b.id(),
                        s.id()
                )).collect(Collectors.joining());
                return String.format(
                        BOOK_SQL_TEMPLATE,
                        b.id(),
                        b.title().replace("'", "''"),
                        b.publisher().replace("'", "''"),
                        b.year(),
                        b.pages(),
                        authors,
                        subfields
                );
            }).collect(Collectors.joining());
            writer.write(sql);
        }
    }

    private static void printBookList(List<Book> bookList) {
        bookList.forEach((Book b) -> System.out.printf(
                PRINT_TEMPLATE,
                b.id(),
                b.title(),
                b.authors()
                        .stream()
                        .map(Author::toString)
                        .collect(Collectors.joining(", ")),
                b.publisher(),
                b.year(),
                b.pages(),
                b.subfields()
                        .stream()
                        .map(Subfield::toString)
                        .collect(Collectors.joining(", "))
        ));
    }
}
