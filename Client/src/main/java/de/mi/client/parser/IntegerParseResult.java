package de.mi.client.parser;

import de.mi.common.Range;
import javafx.beans.property.ReadOnlyProperty;

class IntegerParseResult extends ParseResultBase<Integer> {
    private final Range range;

    IntegerParseResult(ReadOnlyProperty<?> property, Range range) {
        super(property);
        this.range = range;
    }

    @Override
    protected Integer get(boolean throwIfInvalid) {
        String strVal = new StringParseResult(property).get(throwIfInvalid);
        if (strVal == null) return null;

        try {
            int intVal = Integer.parseUnsignedInt(strVal);
            return range != null ? range.checkRange(intVal) : intVal;
        } catch (NumberFormatException e) {
            if (throwIfInvalid) throw createException("is not numeric", e);
            return null;
        } catch (Range.OutOfRangeException e) {
            throw createException("is out of range", e);
        }
    }
}
