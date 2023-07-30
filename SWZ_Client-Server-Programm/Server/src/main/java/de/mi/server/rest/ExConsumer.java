package de.mi.server.rest;

import java.sql.SQLException;

/**
 * Ein {@link FunctionalInterface}, welches das {@link java.util.function.Consumer}-Interface imitiert,
 * mit dem Unterschied, dass die Methode ein {@link SQLException} oder {@link IllegalArgumentException}
 * werfen darf, was für die Ausführung von Datenbank-Anfragen gebraucht wird
 * <p>
 * Erweitert {@link ExFunction}, um an selber Steller genutzt werden zu können
 *
 * @param <T> Der Typ des Parameters
 * @author Malte Kasolowsky <code>m30114</code>
 */
@FunctionalInterface
interface ExConsumer<T> extends ExFunction<Void, T> {

    /**
     * Führt eine Aktion mit dem gegebenen Parameter aus
     *
     * @param t Der Parameter
     * @throws SQLException             Wenn beim Ausführen eine solche Ausnahme aufgetreten ist
     * @throws IllegalArgumentException Wenn beim Ausführen eine solche Ausnahme aufgetreten ist
     */
    void accept(T t) throws SQLException, IllegalArgumentException;

    /**
     * Standard-Methode ist mit der eigenen Methode überschrieben
     */
    @Override
    default Void apply(T t) throws SQLException, IllegalArgumentException {
        accept(t);
        return null;
    }
}
