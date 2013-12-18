package org.cyclop.common;

import com.google.common.base.Objects;
import org.springframework.beans.factory.annotation.Value;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;
import java.io.UnsupportedEncodingException;

/**
 * @author Maciej Miklas
 */
@Named
public class AppConfig {

    public final Cassandra cassandra;

    public final CqlEditor cqlEditor;

    public final Common common;

    public final CqlExport cqlExport;

    private static AppConfig instance = null;

    public static AppConfig get() {
        if (instance == null) {
            throw new IllegalArgumentException("Can not access AppConfig because spring initialization is not trough");
        }
        return instance;
    }

    @PostConstruct
    void init() {
        instance = this;
    }

    private static String crs(String cr, String str) throws UnsupportedEncodingException {
        String replaced = str.replaceAll("CR", String.valueOf((char) 10));
        return replaced;
    }

    @Inject
    public AppConfig(Cassandra cassandra, CqlEditor cqlEditor, Common common, CqlExport cqlExport) {
        this.cassandra = cassandra;
        this.cqlEditor = cqlEditor;
        this.common = common;
        this.cqlExport = cqlExport;
    }

    @Named
    public static class Cassandra {
        public final int resultLimit;

        public final int port;

        public final int timeoutMilis;

        public final String hosts;

        public final boolean useSsl;

        @Inject
        public Cassandra(@Value("${cassandra.resultLimit:10000}") int resultLimit,
                         @Value("${cassandra.hosts}") String hosts, @Value("${cassandra.useSsl}") boolean useSsl,
                         @Value("${cassandra.port}") int port, @Value("${cassandra.timeoutMilis}") int timeoutMilis) {
            this.resultLimit = resultLimit;
            this.hosts = hosts;
            this.useSsl = useSsl;
            this.port = port;
            this.timeoutMilis = timeoutMilis;
        }

        @Override
        public String toString() {
            return Objects.toStringHelper(this).add("resultLimit", resultLimit).add("port", port).add("timeoutMilis",
                    timeoutMilis).add("hosts", hosts).add("useSsl", useSsl).toString();
        }
    }

    @Named
    public static class Common {
    }

    @Named
    public static class CqlEditor {

        public final int rowsPerPage;

        public final int maxColumnEmbeddedDisplayChars;

        public final int maxColumnDisplayChars;

        public final int maxColumnTooltipDisplayChars;

        @Inject
        protected CqlEditor(@Value("${cqlEditor.rowsPerPage}") int rowsPerPage,
                            @Value("${cqlEditor.maxColumnEmbeddedDisplayChars}") int maxColumnEmbeddedDisplayChars,
                            @Value("${cqlEditor.maxColumnDisplayChars}") int maxColumnDisplayChars,
                            @Value("${cqlEditor.maxColumnTooltipDisplayChars}") int maxColumnTooltipDisplayChars) {
            this.rowsPerPage = rowsPerPage;
            this.maxColumnEmbeddedDisplayChars = maxColumnEmbeddedDisplayChars;
            this.maxColumnDisplayChars = maxColumnDisplayChars;
            this.maxColumnTooltipDisplayChars = maxColumnTooltipDisplayChars;
        }

        @Override
        public String toString() {
            return Objects.toStringHelper(this).add("pageLimit", rowsPerPage).add("maxColumnEmbeddedDisplayChars",
                    maxColumnEmbeddedDisplayChars).add("maxColumnDisplayChars",
                    maxColumnDisplayChars).add("maxColumnTooltipDisplayChars", maxColumnTooltipDisplayChars).toString();
        }
    }

    @Override
    public String toString() {
        return Objects.toStringHelper(this).add("cassandra", cassandra).add("cqlEditor", cqlEditor).toString();
    }

    @Named
    public static final class CqlExport {
        public final String querySeparator;

        public final String rowSeparator;

        public final String listSeparator;

        public final String mapSeparator;

        public final String columnSeparator;

        public final int crCharCode;

        public final String valueBracketStart;

        public final String fileName;

        // TODO add javax validation for fields like date
        public final String fileNameDate;

        public final String valueBracketEnd;

        public final boolean trim;

        public final boolean removeCrChars;

        @Inject
        public CqlExport(@Value("${cqlExport.fileName}") String fileName, @Value("${cqlExport.fileName.date}") String
                fileNameDate, @Value("${cqlExport.querySeparator}") String querySeparator,
                         @Value("${cqlExport.rowSeparator}") String rowSeparator,
                         @Value("${cqlExport.columnSeparator}") String columnSeparator,
                         @Value("${cqlExport.listSeparator}") String listSeparator,
                         @Value("${cqlExport.mapSeparator}") String mapSeparator,
                         @Value("${cqlExport.valueBracketStart}") String valueBracketStart,
                         @Value("${cqlExport.valueBracketEnd}") String valueBracketEnd,
                         @Value("${cqlExport.crCharCode}") int crCharCode, @Value("${cqlExport.removeCrChars}")
        boolean removeCrChars, @Value("${cqlExport.trim}") boolean trim) throws UnsupportedEncodingException {
            this.crCharCode = crCharCode;
            String crChar = String.valueOf((char) crCharCode);
            this.querySeparator = crs(crChar, querySeparator);
            this.columnSeparator = crs(crChar, columnSeparator);
            this.rowSeparator = crs(crChar, rowSeparator);
            this.listSeparator = crs(crChar, listSeparator);
            this.mapSeparator = crs(crChar, mapSeparator);
            this.removeCrChars = removeCrChars;
            this.fileName = fileName;
            this.fileNameDate = fileNameDate;
            this.valueBracketStart = valueBracketStart;
            this.valueBracketEnd = valueBracketEnd;
            this.trim = trim;
        }

        @Override
        public String toString() {
            return Objects.toStringHelper(this).add("querySeparator", querySeparator).add("rowSeparator",
                    rowSeparator).add("listSeparator", listSeparator).add("mapSeparator",
                    mapSeparator).add("removeCrChars", removeCrChars).toString();
        }
    }
}

