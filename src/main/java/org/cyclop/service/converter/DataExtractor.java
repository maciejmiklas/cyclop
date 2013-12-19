package org.cyclop.service.converter;

import com.datastax.driver.core.DataType;
import com.datastax.driver.core.Row;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import org.cyclop.model.CqlColumnValue;
import org.cyclop.model.CqlExtendedColumnName;
import org.cyclop.model.CqlPartitionKey;
import org.cyclop.model.CqlPartitionKeyValue;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.inject.Named;
import java.util.Collection;
import java.util.List;
import java.util.Map;

/**
 * @author Maciej Miklas
 */
@Named
public class DataExtractor {
    private final static Logger LOG = LoggerFactory.getLogger(DataExtractor.class);

    public ImmutableList<CqlColumnValue> extractCollection(Row row, CqlExtendedColumnName column) {
        String partLc = column.partLc;
        DataType dataType = column.dataType;
        if (dataType.getName() != DataType.Name.SET && dataType.getName() != DataType.Name.LIST) {
            throw new IllegalArgumentException("Only Collection type is supported");
        }

        List<DataType> argTypes = dataType.getTypeArguments();
        if (argTypes == null || argTypes.isEmpty()) {
            return ImmutableList.of();
        }

        Class<?> collectionClass = argTypes.get(0).asJavaClass();
        Collection<?> objCont = dataType.getName() == DataType.Name.SET ? row.getSet(partLc,
                collectionClass) : row.getList(partLc, collectionClass);

        ImmutableList.Builder<CqlColumnValue> builder = ImmutableList.builder();
        for (Object o : objCont) {
            CqlColumnValue dob = new CqlColumnValue(collectionClass, o, column);
            builder.add(dob);

        }
        ImmutableList<CqlColumnValue> collection = builder.build();
        return collection;
    }

    public ImmutableMap<CqlColumnValue, CqlColumnValue> extractMap(Row row, CqlExtendedColumnName column) {
        String partLc = column.partLc;
        DataType dataType = column.dataType;
        if (dataType.getName() != DataType.Name.MAP) {
            throw new IllegalArgumentException("Only Map type is supported");
        }

        List<DataType> argTypes = dataType.getTypeArguments();
        if (argTypes.size() != 2) {
            return ImmutableMap.of();
        }

        Class<?> keyClass = argTypes.get(0).asJavaClass();
        Class<?> valueClass = argTypes.get(1).asJavaClass();
        Map<?, ?> unconverted = row.getMap(partLc, keyClass, valueClass);

        ImmutableMap.Builder<CqlColumnValue, CqlColumnValue> builder = ImmutableMap.builder();
        for (Map.Entry<?, ?> entry : unconverted.entrySet()) {
            CqlColumnValue dk = new CqlColumnValue(keyClass, entry.getKey(), column);
            CqlColumnValue dv = new CqlColumnValue(valueClass, entry.getValue(), column);
            builder.put(dk, dv);
        }

        ImmutableMap<CqlColumnValue, CqlColumnValue> map = builder.build();
        return map;
    }

    public CqlPartitionKeyValue extractPartitionKey(Row row, CqlPartitionKey partitionKey) {
        CqlColumnValue colSv = extractSingleValue(row, partitionKey);
        CqlPartitionKeyValue key = new CqlPartitionKeyValue(colSv.valueClass, colSv.value, partitionKey);
        return key;
    }

    public CqlColumnValue extractSingleValue(Row row, CqlExtendedColumnName column) {
        String partLc = column.partLc;
        DataType dataType = column.dataType;
        if (dataType.isCollection()) {
            throw new IllegalArgumentException("Collection type is not supported");
        }

        Object extracted = null;
        if (isUUID(dataType)) {
            extracted = row.getUUID(partLc);

        } else if (isString(dataType)) {
            extracted = row.getString(partLc);

        } else if (isLong(dataType)) {
            extracted = row.getLong(partLc);

        } else if (dataType.equals(DataType.cfloat())) {
            extracted = row.getFloat(partLc);

        } else if (dataType.equals(DataType.cint())) {
            extracted = row.getInt(partLc);

        } else if (dataType.equals(DataType.cboolean())) {
            extracted = row.getBool(partLc);

        } else if (dataType.equals(DataType.decimal())) {
            extracted = row.getDecimal(partLc);

        } else if (dataType.equals(DataType.cdouble())) {
            extracted = row.getDouble(partLc);

        } else if (dataType.equals(DataType.varint())) {
            extracted = row.getVarint(partLc);

        } else if (dataType.equals(DataType.timestamp())) {
            extracted = row.getDate(partLc);

        } else if (dataType.equals(DataType.inet())) {
            extracted = row.getInet(partLc);
        } else {
            LOG.warn("Type: " + dataType + " not supported by data converter");
        }

        if (extracted == null) {
            extracted = "";
        }

        Class<?> eClass = extracted.getClass();
        CqlColumnValue cqlColumnValue = new CqlColumnValue(eClass, extracted, column);
        return cqlColumnValue;
    }

    private boolean isUUID(DataType dataType) {
        return dataType.equals(DataType.uuid()) || dataType.equals(DataType.timeuuid());
    }

    private boolean isLong(DataType dataType) {
        return dataType.equals(DataType.bigint()) || dataType.equals(DataType.counter());
    }

    private boolean isString(DataType dataType) {
        return dataType.equals(DataType.ascii()) || dataType.equals(DataType.text()) || dataType.equals(DataType
                .varchar());
    }
}
