package de.mi.server.mapper;

import de.mi.common.Author;

import java.util.Map;
import java.util.Optional;

class AuthorMapper implements Mapper<Author> {

    /**
     * Erzeugt eine neue Instanz aus den Werten,
     * welche in der {@link Map} gespeichert und denen entsprechende Namen zugeordnet sind
     *
     * @param values Die Map, welche die zu übertragenden Werte beinhaltet
     * @return Eine neue Instanz aus den Werten aus der Map
     */
    @Override
    public Author apply(Map<String, Object> values) {
        return new Author((String) values.get("first_name"), (String) values.get("last_name"));
    }
}
