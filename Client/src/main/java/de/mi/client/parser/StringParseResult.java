package de.mi.client.parser;

import javafx.beans.property.ReadOnlyProperty;

class StringParseResult extends ParseResultBase<String> {

    StringParseResult(ReadOnlyProperty<?> property) {
        super(property);
    }

    @Override
    protected String get(boolean throwIfInvalid) {
        Object value = property.getValue();
        if (value == null) {
            if (throwIfInvalid) throw createException("is null", null);
            return null;
        }

        String strVal = value instanceof String s ? s : "";
        if (strVal.isBlank()) {
            if (throwIfInvalid) throw createException("is blank", null);
            return null;
        }

        return strVal;
    }
}
