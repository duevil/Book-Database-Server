package de.mi.mapper;

import de.mi.common.Subfield;

import java.util.Map;

public class SubfieldMapper implements Mapper<Subfield> {
    private static final Subfield NULL_SUBFIELD = new Subfield(-1, null);

    /**
     * Erzeugt eine neue Instanz aus den Werten,
     * welche in der {@link Map} gespeichert und denen entsprechende Namen zugeordnet sind
     *
     * @param values Die Map, welche die zu übertragenden Werte beinhaltet
     * @return Eine neue Instanz aus den Werten aus der Map
     */
    @Override
    public Subfield apply(Map<String, Object> values) {
        return null; // TODO: add mapping
    }
}
