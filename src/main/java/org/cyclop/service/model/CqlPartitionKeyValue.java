package org.cyclop.service.model;

import com.google.common.base.Objects;

/**
 * @author Maciej Miklas
 */
public class CqlPartitionKeyValue extends CqlColumnValue {

    public final CqlPartitionKey cqlPartitionKey;

    public CqlPartitionKeyValue(Class<?> valueClass, Object value, CqlPartitionKey cqlPartitionKey) {
        super(valueClass, value, cqlPartitionKey);
        this.cqlPartitionKey = cqlPartitionKey;
    }

    @Override
    public String toString() {
        return Objects.toStringHelper(this).add("valueClass", valueClass).add("value", value).add("cqlPartitionKey",
                cqlPartitionKey).toString();
    }
}
