package de.mi.mapper;

import java.util.Map;
import java.util.function.Function;

/**
 * Interface zum Mappen einer {@link Map} mit Objekten, welche einem Namen zugeordnet sind,
 * auf eine neue Instanz eines Objektes aus den Werten aus der Map
 *
 * @see java.util.function.Function
 * @author Malte Kasolowsky <code>m30114</code>
 * @param <T> Der Typ des Resultats des Mappings
 */
public interface Mapper<T> extends Function<Map<String, Object>, T> {

    /**
     * Erzeugt eine neue Instanz aus den Werten,
     * welche in der {@link Map} gespeichert und denen entsprechende Namen zugeordnet sind
     *
     * @param values Die Map, welche die zu übertragenden Werte beinhaltet
     * @return Eine neue Instanz aus den Werten aus der Map
     */
    @Override
    T apply(Map<String, Object> values);
}
