package de.mi.mapper;

import de.mi.common.Author;

import java.util.Map;
import java.util.Optional;

class AuthorMapper implements Mapper<Author> {
    private static final Author NULL_AUTHOR = new Author(-1, null, null);

    /**
     * Erzeugt eine neue Instanz aus den Werten,
     * welche in der {@link Map} gespeichert und denen entsprechende Namen zugeordnet sind
     *
     * @param values Die Map, welche die zu übertragenden Werte beinhaltet
     * @return Eine neue Instanz aus den Werten aus der Map
     */
    @Override
    public Author apply(Map<String, Object> values) {
        return Optional.ofNullable((Integer) values.get("id"))
                .map(id -> new Author(
                        id,
                        (String) values.get("first_name"),
                        (String) values.get("last_name")
                ))
                .orElse(NULL_AUTHOR);
    }
}
