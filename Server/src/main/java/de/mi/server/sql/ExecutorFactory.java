package de.mi.server.sql;

import java.io.InputStream;
import java.sql.PreparedStatement;
import java.sql.Statement;
import java.util.List;

/**
 * Utility-Klasse zum Erzeugen von entsprechenden {@link Executor}
 *
 * @author Malte Kasolowsky <code>m30114</code>
 */
public final class ExecutorFactory {

    /**
     * Privater Konstruktor; eine Erzeugung einer Klassen-Instanz ist nicht nötig
     */
    private ExecutorFactory() {
    }

    /**
     * Erzeugt einen neuen {@link Executor} zum Laden von Daten aus der Datenbank
     * mittels eines {@link PreparedStatement PreparedStatements}
     *
     * @param statement Das Statement, welches vom Executor ausgeführt werden soll
     * @param mapper    Ein {@link Mapper}, mit dem die geladenen Daten geparsed werden sollen
     * @param <V>       Der Typ der zu ladenden Objekte
     * @return Ein neuen Executor aus den entsprechenden Argumenten
     */
    public static <V> Executor<List<V>> createQuery(PreparedStatement statement, Mapper<V> mapper) {
        return new QueryExecutor<>(statement, mapper);
    }

    /**
     * Erzeugt einen neuen {@link Executor} zum Laden von Daten aus der Datenbank
     * mittels eines {@link Statement Statements} und eines SQL-Befehls
     *
     * @param statement Das Statement, welches vom Executor ausgeführt werden soll
     * @param sql       Der SQL-Befehl, welcher vom Executor ausgeführt werden soll
     * @param mapper    Ein {@link Mapper}, mit dem die geladenen Daten geparsed werden sollen
     * @param <V>       Der Typ der zu ladenden Objekte
     * @return Ein neuen Executor aus den entsprechenden Argumenten
     */
    public static <V> Executor<List<V>> createQuery(Statement statement, String sql, Mapper<V> mapper) {
        return new QueryExecutor<>(statement, sql, mapper);
    }

    /**
     * Erzeugt einen neuen {@link Executor} zum Manipulieren von Daten in der Datenbank
     * mittels eines {@link PreparedStatement PreparedStatments}
     *
     * @param statement Das Statement, welches vom Executor ausgeführt werden soll
     * @return Ein neuen Executor aus den entsprechenden Argumenten
     */
    public static Executor<Integer> createUpdater(PreparedStatement statement) {
        return new UpdateExecutor(statement);
    }

    /**
     * Erzeugt einen neuen {@link Executor} zum Ausführen eines {@link InputStream}, welcher mehrere Befehle enthält,
     * wie z.B. eine .sql-Datei, mittels eines {@link Statement Statements}
     *
     * @param statement Das Statement, welches vom Executor ausgeführt werden soll
     * @param data      Ein Datenstrom mit den SQl-Befehlen, die vom Statement ausgeführt werden sollen
     * @return Ein neuen Executor aus den entsprechenden Argumenten
     */
    public static Executor<Void> createScriptRunner(Statement statement, InputStream data) {
        return new ScriptRunner(statement, data);
    }
}
