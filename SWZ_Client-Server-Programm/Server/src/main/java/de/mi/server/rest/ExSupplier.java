package de.mi.server.rest;

import java.sql.SQLException;


/**
 * Ein {@link FunctionalInterface}, welches das {@link java.util.function.Supplier}-Interface imitiert,
 * mit dem Unterschied, dass die Methode ein {@link SQLException} oder {@link IllegalArgumentException}
 * werfen darf, was für die Ausführung von Datenbank-Anfragen gebraucht wird
 * <p>
 * Erweitert {@link ExFunction}, um an selber Steller genutzt werden zu können
 *
 * @param <T> Der Typ des Parameters
 * @author Malte Kasolowsky <code>m30114</code>
 */
@FunctionalInterface
interface ExSupplier<T> extends ExFunction<T, Void> {

    /**
     * Gibt einen Wert zurück
     *
     * @return Einen Wert
     * @throws SQLException             Wenn beim Ausführen eine solche Ausnahme aufgetreten ist
     * @throws IllegalArgumentException Wenn beim Ausführen eine solche Ausnahme aufgetreten ist
     */
    T get() throws SQLException, IllegalArgumentException;

    /**
     * Standard-Methode ist mit der eigenen Methode überschrieben
     */
    @Override
    default T apply(Void ignored) throws SQLException, IllegalArgumentException {
        return get();
    }
}
