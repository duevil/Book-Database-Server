package de.mi.server.mapper;

import de.mi.common.Book;

import java.util.Map;

class BookMapper implements Mapper<Book> {

    /**
     * Erzeugt eine neue Instanz aus den Werten,
     * welche in der {@link Map} gespeichert und denen entsprechende Namen zugeordnet sind
     *
     * @param values Die Map, welche die zu übertragenden Werte beinhaltet
     * @return Eine neue Instanz aus den Werten aus der Map
     */
    @Override
    public Book apply(Map<String, Object> values) {
        return new Book(
                (String) values.get("title"),
                null,
                (String) values.get("publisher"),
                (Integer) values.get("year"),
                (Integer) values.get("pages"),
                (Integer) values.get("rating"),
                null
        );
    }
}
