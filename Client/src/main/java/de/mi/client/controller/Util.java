package de.mi.client.controller;

import de.mi.common.Range;
import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.property.ReadOnlyProperty;
import javafx.beans.property.StringProperty;
import javafx.scene.control.TextFormatter;
import javafx.scene.control.TextInputControl;
import javafx.util.converter.IntegerStringConverter;

import java.util.Optional;
import java.util.function.Supplier;
import java.util.function.UnaryOperator;
import java.util.regex.Pattern;

final class Util {
    private static final Pattern REGEX = Pattern.compile("^\\d*$");
    private static final UnaryOperator<TextFormatter.Change> FILTER
            = c -> REGEX.matcher(c.getControlNewText()).matches() ? c : null;

    private Util() {
    }

    public static void bindProperties(Property<Integer> firstProperty, StringProperty secondProperty) {
        Bindings.bindBidirectional(secondProperty, firstProperty, new IntegerStringConverter());
    }

    public static <T> void bindProperties(Property<T> firstProperty, Property<T> secondProperty) {
        Bindings.bindBidirectional(firstProperty, secondProperty);
    }

    public static void addFormatter(TextInputControl control) {
        control.setTextFormatter(new TextFormatter<>(FILTER));
    }

    public static <T> T readProperty(ReadOnlyProperty<T> property) throws PropertyException {
        return new ReadResult<>(property).get();
    }

    public static <T> Optional<T> readPropertyOptional(ReadOnlyProperty<T> property) {
        return new ReadResult<>(property).getOptional();
    }

    public static Integer readProperty(ReadOnlyProperty<Integer> property, Range range)
            throws PropertyException {
        return readPropertyOptional(property, range).getRanged().orElse(null);
    }

    public static RangedOptional readPropertyOptional(ReadOnlyProperty<Integer> property, Range range)
            throws PropertyException {
        return new RangedOptional(property, range);
    }

    public static Supplier<PropertyException> propertyExceptionSupplier(ReadOnlyProperty<?> property) {
        return () -> new PropertyException(property, null);
    }

    public static final class PropertyException extends IllegalStateException {
        private final transient ReadOnlyProperty<?> property;

        private PropertyException(ReadOnlyProperty<?> property, Throwable cause) {
            super(property.getName(), cause);
            this.property = property;
        }

        @SuppressWarnings("unchecked")
        public <T> ReadOnlyProperty<T> getProperty() {
            return (ReadOnlyProperty<T>) property;
        }
    }

    private static class ReadResult<T> {
        private final ReadOnlyProperty<T> property;

        protected ReadResult(ReadOnlyProperty<T> property) {
            this.property = property;
        }

        private T get() {
            return getOptional().orElseThrow(this::exception);
        }

        private Optional<T> getOptional() {
            return Optional.of(property).map(ReadOnlyProperty::getValue);
        }

        private PropertyException exception() {
            return new PropertyException(property, new IllegalStateException("is null"));
        }
    }

    public static final class RangedOptional extends ReadResult<Integer> {
        private final Range range;

        private RangedOptional(ReadOnlyProperty<Integer> property, Range range) {
            super(property);
            this.range = range;
        }

        private Optional<Integer> getRanged() {
            try {
                return super.getOptional().map(range::checkRange);
            } catch (Range.OutOfRangeException e) {
                throw new PropertyException(super.property, e);
            }
        }

        public Integer orMin() {
            return getRanged().orElse(range.min());
        }

        public Integer orMax() {
            return getRanged().orElse(range.max());
        }
    }
}
