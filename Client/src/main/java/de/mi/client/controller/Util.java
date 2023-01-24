package de.mi.client.controller;

import de.mi.common.Range;
import javafx.beans.binding.Bindings;
import javafx.beans.binding.BooleanExpression;
import javafx.beans.binding.When;
import javafx.beans.property.Property;
import javafx.beans.property.ReadOnlyProperty;
import javafx.beans.property.StringProperty;
import javafx.beans.value.ObservableBooleanValue;
import javafx.beans.value.ObservableValue;
import javafx.scene.Node;
import javafx.scene.control.Label;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.TextFormatter;
import javafx.scene.control.TextInputControl;
import javafx.scene.paint.Color;
import javafx.util.StringConverter;
import javafx.util.converter.IntegerStringConverter;

import java.util.Optional;
import java.util.function.Supplier;
import java.util.function.UnaryOperator;
import java.util.regex.Pattern;

/**
 * Utility-Klasse mit einfachen Methoden für Controller-Klassen,
 * wie bsp. das Auslesen oder Verknüpfen von {@link Property Properties}
 *
 * @author Malte Kasolowsky <code>m30114</code>
 */
final class Util {
    private static final Pattern REGEX = Pattern.compile("^\\d{0,5}$");
    private static final UnaryOperator<TextFormatter.Change> FILTER
            = c -> REGEX.matcher(c.getControlNewText()).matches() ? c : null;
    private static final Color COLOR = Color.GRAY.interpolate(Color.LIGHTGRAY, 0.75);

    /**
     * Privater Konstruktor; eine Erzeugung einer Klassen-Instanz ist nicht nötig
     */
    private Util() {
    }

    /**
     * {@link Bindings#bindBidirectional(Property, Property, StringConverter) Verknüpft} eine {@link StringProperty}
     * und eine Integer-{@link Property} mit einem {@link IntegerStringConverter}
     *
     * @param firstProperty  Die zu verknüpfende Integer-Property
     * @param secondProperty Die zu verknüpfende String-Property
     */
    public static void bindProperties(Property<Integer> firstProperty, StringProperty secondProperty) {
        Bindings.bindBidirectional(secondProperty, firstProperty, new IntegerStringConverter());
    }

    /**
     * {@link Bindings#bindBidirectional(Property, Property) Verknüpft} zwei {@link Property Properties} miteinander
     *
     * @param firstProperty  Die erste zu verknüpfende Property
     * @param secondProperty Die zweite zu verknüpfende Property
     * @param <T>            Der Typ der Properties
     */
    public static <T> void bindProperties(Property<T> firstProperty, Property<T> secondProperty) {
        Bindings.bindBidirectional(firstProperty, secondProperty);
    }

    /**
     * {@link Property#bind(ObservableValue) Knüpft} die {@link ScrollPane#contentProperty() Inhalts-Property}
     * einer {@link ScrollPane} über ein {@link When}-Binding an ein {@link Label} oder einer {@link Node}
     * je nach Zustande einer gegebenen {@link BooleanExpression Kondition}
     *
     * @param pane      Die ScrollPane, deren Inhalt gebunden werden soll
     * @param content   Das Element, der angezeigt werden soll, wenn die Kondition zutrifft
     * @param labelText Der Text, der als Label angezeigt werden soll, wenn die Kondition <i>nicht</i> zutrifft
     * @param condition Die Kondition für das {@link When#When(ObservableBooleanValue) When-Statement}
     */
    public static void bindScrollPaneContentOrLabel(ScrollPane pane,
                                                    Node content,
                                                    String labelText,
                                                    BooleanExpression condition) {
        var label = new Label(labelText);
        label.setTextFill(COLOR);
        pane.contentProperty().bind(new When(condition).then(content).otherwise(label));
    }

    /**
     * Fügt einem {@link TextInputControl} einen {@link TextFormatter} hinzu,
     * welcher die Eingabe von Nicht-Zahlen-Zeichen und zu langen Zahlen verhindert
     *
     * @param control Das Text-Element, welchem ein Formatter gegeben werden soll
     */
    public static void addFormatter(TextInputControl control) {
        control.setTextFormatter(new TextFormatter<>(FILTER));
    }

    /**
     * Liest den Wert einer {@link ReadOnlyProperty} aus und wirft eine Ausnahme,
     * wenn der Wert null oder, wenn der Inhalt der Property ein String ist, dieser {@link String#isBlank() leer} ist
     *
     * @param property Die auszulesende Property
     * @param <T>      Der Typ des Property-Wertes
     * @return Der aktuelle Wert der Property, falls dieser nicht
     * @throws PropertyException Wenn der Wert der Property null
     *                           oder für den Fall, dass es ein String ist, dieser leer ist
     */
    public static <T> T readProperty(ReadOnlyProperty<T> property) throws PropertyException {
        return new ReadResult<>(property).get();
    }

    /**
     * Liest den Wert einer {@link ReadOnlyProperty} aus und gibt den Wert in einem {@link Optional} zurück,
     * oder ein {@link Optional#empty() leeres Optional},
     * wenn der Wert null oder, wenn der Inhalt der Property ein String ist, dieser {@link String#isBlank() leer} ist
     *
     * @param property Die auszulesende Property
     * @param <T>      Der Typ des Property-Wertes
     * @return Ein Optional mit dem Wert der Property oder ein leeres Optional,
     * wenn der Wert null oder, wenn der Wert ein String ist, dieser leer ist
     */
    public static <T> Optional<T> readPropertyOptional(ReadOnlyProperty<T> property) {
        return new ReadResult<>(property).getOptional();
    }

    /**
     * Liest den Wert einer Integer-{@link ReadOnlyProperty} aus und wirft eine Ausnahme,
     * wenn der Wert null ist
     * <p>
     * Der ausgelesene Wert wird zudem gegen eine gegebene {@link Range} {@link Range#checkRange(int)} geprüft,
     * und eine {@link PropertyException} in dem Fall geworfen,
     * dass der ausgelesene Wert nicht innerhalb des Bereiches liegt
     *
     * @param property Die auszulesende Property
     * @param range    Der Bereich, für den die Property ausgelesen werden soll
     * @return Der aktuelle Wert der Property, falls dieser nicht
     * @throws PropertyException Wenn der Wert der Property null
     *                           oder außerhalb des gegebenen Bereiches liegt
     */
    public static Integer readProperty(ReadOnlyProperty<Integer> property, Range range)
            throws PropertyException {
        return readPropertyOptional(property, range).get();
    }

    public static RangedOptional readPropertyOptional(ReadOnlyProperty<Integer> property, Range range)
            throws PropertyException {
        return new RangedOptional(property, range);
    }

    /**
     * Erstellt einen {@link Supplier}, welcher eine neue {@link Util.PropertyException}
     * für eine gegebene {@link ReadOnlyProperty} erzeugt
     *
     * @param property Die Property, für die die Ausnahme erzeugt werden kann
     * @return Ein Supplier zum Erzeugen einer entsprechenden Ausnahme
     */
    public static Supplier<PropertyException> emptyPropertyExceptionSupplier(ReadOnlyProperty<?> property) {
        return () -> new PropertyException(property, new IllegalStateException("Value must not be empty"));
    }

    /**
     * Erstellt eine neue {@link Util.PropertyException} ausgehend vom gegebenen Namen einer Property
     * und der Ursache für die Ausnahme
     *
     * @param name  Der Name einer Property, die die Ursache für die Ausnahme darstellt
     * @param cause Die Ursache der Ausnahme
     * @return Eine neue PropertyException aus den gegebenen Argumenten
     */
    public static PropertyException createPropertyException(String name, Throwable cause) {
        return new PropertyException(name, cause);
    }

    /**
     * Ausnahme für das Anzeigen eines fehlerhaften (meist null-wertigen oder leeren) {@link Property}-Wertes
     */
    public static final class PropertyException extends IllegalStateException {

        /**
         * Konstruktor; erstellt eine neue Ausnahme mit dem {@link Property#getName() Namen der gegebenen Property}
         * und einer {@link Throwable} als Ursache
         *
         * @param property Die Property, für die die Ausnahme erstellt wird
         * @param cause    Die Ursache der Ausnahme; kann null sein
         */
        private PropertyException(ReadOnlyProperty<?> property, Throwable cause) {
            this(property.getName(), cause);
        }

        /**
         * Konstruktor; erstellt eine neue Ausnahme mit dem gegebenen Property-Namen
         * und einer {@link Throwable} als Ursache
         *
         * @param name  Der Name der Property, die für die Ausnahme verantwortlich ist
         * @param cause Die Ursache der Ausnahme; kann null sein
         */
        private PropertyException(String name, Throwable cause) {
            super(name, cause);
        }
    }

    /**
     * Erweitert die Klasse {@link ReadResult} für Integer-{@link Property Properties}
     * um die Überprüfung des Wertes auf eine {@link Range}
     */
    public static final class RangedOptional extends ReadResult<Integer> {
        private final Range range;

        /**
         * Konstruktor; speichert die auszulesende Integer-{@link Property}
         * und die {@link Range}, mit der der ausgelesene Wert geprüft werden soll
         *
         * @param property Die Property, deren Integer-Wert ausgelesen werden soll
         * @param range    Der Bereich, für den geprüft werden soll, ob der Wert sich innerhalb von diesem befindet
         */
        private RangedOptional(ReadOnlyProperty<Integer> property, Range range) {
            super(property);
            this.range = range;
        }

        /**
         * Gibt den Wert der ausgelesenen {@link Property} oder,
         * sollte der Wert null sein, die untere Grenze der gespeicherten {@link Range}
         *
         * @return Den Wert der Property oder den die untere Grenze des Bereichs
         * @throws PropertyException Wenn der Wert außerhalb des gespeicherten Bereichs liegt
         */
        public Integer orMin() throws PropertyException {
            return getOptional().orElse(range.min());
        }

        /**
         * Gibt den Wert der ausgelesenen {@link Property} oder,
         * sollte der Wert null sein, die obere Grenze der gespeicherten {@link Range}
         *
         * @return Den Wert der Property oder den die obere Grenze des Bereichs
         * @throws PropertyException Wenn der Wert außerhalb des gespeicherten Bereichs liegt
         */
        public Integer orMax() throws PropertyException {
            return getOptional().orElse(range.max());
        }

        /**
         * Ist der Wert der gespeicherten {@link Property} nicht null,
         * so wird geprüft, ob dieser {@link Range#checkRange(int) innerhalb der gespeicherten Range} ist
         *
         * @return Die Rückgabe von {@link ReadResult#getOptional()}, sofern der Wert innerhalb des Bereiches ist
         * @throws PropertyException Wenn der Wert außerhalb des gespeicherten Bereichs liegt
         */
        @Override
        protected Optional<Integer> getOptional() throws PropertyException {
            try {
                return super.getOptional().map(range::checkRange);
            } catch (Range.OutOfRangeException e) {
                throw new PropertyException(super.property, e);
            }
        }
    }

    /**
     * Klasse zur einfacheren Handhabung des Auslesens einer {@link Property}
     *
     * @param <T> Der Typ der Property
     */
    private static class ReadResult<T> {
        private final ReadOnlyProperty<T> property;

        /**
         * Konstruktor; speicher die auszulesende {@link Property}
         *
         * @param property Die Property, dessen Wert ausgelesen werden soll
         */
        protected ReadResult(ReadOnlyProperty<T> property) {
            this.property = property;
        }

        /**
         * Gibt den Wert der {@link Property} zurück;
         * genauer, gibt den Wert von {@link ReadResult#getOptional()} zurück,
         * sofern dieser existent ist,
         * und wirft andernfalls eine {@link Util#emptyPropertyExceptionSupplier(ReadOnlyProperty) Ausnahme}
         * <p>
         * Ist der Wert der Property zudem ein String so wird ebenfalls geprüft,
         * ob der String nicht {@link String#isBlank() leer ist} und alle führenden oder abschließende Leerzeichen
         * {@link String#trim() entfernt}
         *
         * @return Den Wert der Property; dieser wird nicht null sein
         * @throws PropertyException Wenn der Wert der Property null oder leer ist
         */
        protected T get() throws PropertyException {
            return getOptional().map(this::testString).orElseThrow(emptyPropertyExceptionSupplier(property));
        }

        /**
         * Lies den Wert der gespeicherten {@link Property} aus und verpackt diesen in einem {@link Optional}
         *
         * @return Ein Optional, welche den Wert der Property trägt, oder leer ist, sollte der Wert null sein
         */
        protected Optional<T> getOptional() {
            return Optional.ofNullable(property.getValue());
        }

        @SuppressWarnings("unchecked")
        private T testString(T t) {
            if (t instanceof String s) return s.isBlank() ? null : (T) s.trim();
            else return t;
        }
    }
}
