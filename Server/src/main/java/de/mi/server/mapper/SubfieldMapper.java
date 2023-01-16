package de.mi.server.mapper;

import de.mi.common.Subfield;

import java.util.Map;
import java.util.Optional;

class SubfieldMapper implements Mapper<Subfield> {

    /**
     * Erzeugt eine neue Instanz aus den Werten,
     * welche in der {@link Map} gespeichert und denen entsprechende Namen zugeordnet sind
     *
     * @param values Die Map, welche die zu übertragenden Werte beinhaltet
     * @return Eine neue Instanz aus den Werten aus der Map
     */
    @Override
    public Subfield apply(Map<String, Object> values) {
        return new Subfield((String) values.get("name"));
    }
}
