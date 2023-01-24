package de.mi.server.rest;

import java.sql.SQLException;

/**
 * Ein {@link FunctionalInterface}, welches das {@link java.util.function.Function}-Interface imitiert,
 * mit dem Unterschied, dass die Methode ein {@link SQLException} oder {@link IllegalArgumentException}
 * werfen darf, was für die Ausführung von Datenbank-Anfragen gebraucht wird
 *
 * @param <R> Der Typ des Resultats der Funktion
 * @param <T> Der Typ des Funktionsparameters
 * @author Malte Kasolowsky <code>m30114</code>
 */
@FunctionalInterface
interface ExFunction<R, T> {

    /**
     * Führt eine Aktion mit dem gegebenen Parameter aus und das Resultat der Aktion zurück
     *
     * @param t Der Parameter
     * @return Das Resultat
     * @throws SQLException             Wenn beim Ausführen eine solche Ausnahme aufgetreten ist
     * @throws IllegalArgumentException Wenn beim Ausführen eine solche Ausnahme aufgetreten ist
     */
    R apply(T t) throws SQLException, IllegalArgumentException;
}
