package org.cyclop.service.converter;

import com.datastax.driver.core.DataType;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;
import org.cyclop.common.AppConfig;
import org.cyclop.model.*;

import javax.inject.Inject;
import javax.inject.Named;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

/**
 * @author Maciej Miklas
 */
@Named
public class CsvQueryResultExporter {

    @Inject
    private DataExtractor extractor;

    @Inject
    private DataConverter converter;

    @Inject
    private AppConfig.CqlExport conf;

    public String createCsv(CqlQuery query, CqlSelectResult result) {
        StringBuilder buf = new StringBuilder();

        // header
        appendHeader(query, buf);

        // column names
        List<CqlExtendedColumnName> cols = new ArrayList<>(result.commonColumns.size() + result.commonColumns.size());
        appendColumns(buf, result.commonColumns, cols);
        appendGroupSeparator(result, buf);
        appendColumns(buf, result.dynamicColumns, cols);

        buf.append(conf.rowSeparator);

        // content
        for (CqlRow row : result.rows) {
            appendRow(buf, row, cols);
            buf.append(conf.rowSeparator);
        }
        return buf.toString();
    }

    private void appendRow(StringBuilder buf, CqlRow row, List<CqlExtendedColumnName> cols) {
        Iterator<CqlExtendedColumnName> it = cols.iterator();
        while (it.hasNext()) {
            CqlExtendedColumnName column = it.next();
            DataType dataType = column.dataType;

            if (dataType.getName() == DataType.Name.SET || dataType.getName() == DataType.Name.LIST) {
                appendCollection(buf, row, column);

            } else if (dataType.getName() == DataType.Name.MAP) {
                appendMap(buf, row, column);
            } else {
                appendSingleValue(buf, row, column);
            }

            if (it.hasNext()) {
                buf.append(conf.columnSeparator);
            }
        }
    }

    private void appendMap(StringBuilder buf, CqlRow row, CqlExtendedColumnName column) {
        ImmutableSet<Map.Entry<CqlColumnValue, CqlColumnValue>> displayMap = extractor.extractMap(row.original,
                column).entrySet();
        Iterator<Map.Entry<CqlColumnValue, CqlColumnValue>> it = displayMap.iterator();

        StringBuilder mapBuf = new StringBuilder();
        while (it.hasNext()) {
            Map.Entry<CqlColumnValue, CqlColumnValue> entry = it.next();

            CqlColumnValue key = entry.getKey();
            String keyText = esc(converter.convert(key.value));
            mapBuf.append(keyText);

            mapBuf.append(conf.mapSeparator);

            CqlColumnValue val = entry.getValue();
            String valText = esc(converter.convert(val.value));
            mapBuf.append(valText);

            if (it.hasNext()) {
                mapBuf.append(conf.listSeparator);
            }
        }

        buf.append(esc(mapBuf.toString()));
    }

    private void appendCollection(StringBuilder buf, CqlRow row, CqlExtendedColumnName column) {
        ImmutableList<CqlColumnValue> content = extractor.extractCollection(row.original, column);
        Iterator<CqlColumnValue> contentIt = content.iterator();
        StringBuilder listBuild = new StringBuilder();
        while (contentIt.hasNext()) {
            CqlColumnValue cqlColumnValue = contentIt.next();
            String valText = esc(converter.convert(cqlColumnValue.value));
            listBuild.append(valText);
            if (contentIt.hasNext()) {
                listBuild.append(conf.listSeparator);
            }
        }
        buf.append(esc(listBuild.toString()));
    }

    private void appendSingleValue(StringBuilder buf, CqlRow row, CqlExtendedColumnName column) {
        CqlColumnValue cqlColumnValue = extractor.extractSingleValue(row.original, column);
        String valText = esc(converter.convert(cqlColumnValue.value));
        buf.append(valText);
    }

    private void appendHeader(CqlQuery query, StringBuilder buf) {
        buf.append(prep(query.cql));
        buf.append(conf.querySeparator);
    }

    private void appendGroupSeparator(CqlSelectResult result, StringBuilder buf) {
        if (!result.commonColumns.isEmpty() && !result.dynamicColumns.isEmpty()) {
            buf.append(conf.columnSeparator);
        }
    }

    private void appendColumns(StringBuilder buf, List<CqlExtendedColumnName> columns,
                               List<CqlExtendedColumnName> ret) {
        if (columns.isEmpty()) {
            return;
        }

        Iterator<CqlExtendedColumnName> commonColsIt = columns.iterator();
        while (commonColsIt.hasNext()) {
            CqlExtendedColumnName next = commonColsIt.next();
            ret.add(next);
            buf.append(prep(esc(next.part)));

            if (commonColsIt.hasNext()) {
                buf.append(conf.columnSeparator);
            }
        }
    }

    private String prep(String val) {
        if (val == null) {
            val = "";
        }
        if (conf.trim) {
            val = val.trim();
        }

        if (conf.removeCrChars) {
            val = val.replaceAll("[\n\r]", "");
        }

        return val;
    }

    private String esc(String val) {
        return conf.valueBracketStart + prep(val) + conf.valueBracketEnd;
    }
}
