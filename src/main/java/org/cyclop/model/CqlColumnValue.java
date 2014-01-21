package org.cyclop.model;

import com.google.common.base.Objects;
import javax.annotation.concurrent.Immutable;

/**
 * @author Maciej Miklas
 */
@Immutable
public class CqlColumnValue {

    public final Class<?> valueClass;

    public final Object value;

    public final CqlExtendedColumnName columnName;

    public CqlColumnValue(Class<?> valueClass, Object value, CqlExtendedColumnName columnName) {
        this.valueClass = valueClass;
        this.value = value;
        this.columnName = columnName;
    }

    @Override
    public String toString() {
        return Objects.toStringHelper(this).add("valueClass", valueClass).add("value", value).add("columnName",
                columnName).toString();
    }
}
