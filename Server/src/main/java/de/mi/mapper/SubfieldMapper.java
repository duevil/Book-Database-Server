package de.mi.mapper;

import de.mi.common.Subfield;

import java.util.Map;
import java.util.Optional;

class SubfieldMapper implements Mapper<Subfield> {
    private static final Subfield NULL_SUBFIELD = new Subfield(0, null);

    /**
     * Erzeugt eine neue Instanz aus den Werten,
     * welche in der {@link Map} gespeichert und denen entsprechende Namen zugeordnet sind
     *
     * @param values Die Map, welche die zu übertragenden Werte beinhaltet
     * @return Eine neue Instanz aus den Werten aus der Map
     */
    @Override
    public Subfield apply(Map<String, Object> values) {
        return Optional.ofNullable((Integer) values.get("id"))
                .map(id -> new Subfield(
                        id,
                        (String) values.get("name")
                ))
                .orElse(NULL_SUBFIELD);
    }
}
