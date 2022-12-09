package de.mi.mapper;

import de.mi.common.Book;

import java.util.Map;
import java.util.Optional;

class BookMapper implements Mapper<Book> {
    private static final Book NULL_BOOK = new Book(-1, null, null, null, -1, -1, null);

    /**
     * Erzeugt eine neue Instanz aus den Werten,
     * welche in der {@link Map} gespeichert und denen entsprechende Namen zugeordnet sind
     *
     * @param values Die Map, welche die zu übertragenden Werte beinhaltet
     * @return Eine neue Instanz aus den Werten aus der Map
     */
    @Override
    public Book apply(Map<String, Object> values) {
        return Optional.ofNullable((Integer) values.get("id"))
                .map(id -> new Book(
                        id,
                        (String) values.get("title"),
                        null,
                        (String) values.get("publisher"),
                        (Integer) values.get("year"),
                        (Integer) values.get("pages"),
                        null
                ))
                .orElse(NULL_BOOK);
    }
}
