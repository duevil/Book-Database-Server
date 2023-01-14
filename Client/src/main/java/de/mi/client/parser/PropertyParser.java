package de.mi.client.parser;

import de.mi.common.Range;
import javafx.beans.property.ReadOnlyProperty;

public final class PropertyParser {
    private PropertyParser() {
    }

    public static ParseResult<String> parseString(ReadOnlyProperty<?> property) {
        return new StringParseResult(property);
    }

    public static ParseResult<Integer> parseInteger(ReadOnlyProperty<?> property) {
        return parseInteger(property, null);
    }

    public static ParseResult<Integer> parseInteger(ReadOnlyProperty<?> property, Range range) {
        return new IntegerParseResult(property, range);
    }
}
