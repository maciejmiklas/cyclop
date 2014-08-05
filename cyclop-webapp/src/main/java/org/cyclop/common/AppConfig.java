/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.cyclop.common;

import java.io.Serializable;
import java.io.UnsupportedEncodingException;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;
import javax.validation.Valid;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;

import net.jcip.annotations.Immutable;

import org.apache.commons.lang.Validate;
import org.cyclop.model.exception.ServiceException;
import org.cyclop.validation.BeanValidator;
import org.cyclop.validation.SimpleDate;
import org.hibernate.validator.constraints.NotEmpty;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;

import com.google.common.base.Objects;

/** @author Maciej Miklas */
@Named
public final class AppConfig implements Serializable {
    private final static Logger LOG = LoggerFactory.getLogger(AppConfig.class);

    @NotNull
    @Valid
    private static AppConfig instance = null;

    @NotNull
    @Valid
    public final Cassandra cassandra;

    @NotNull
    @Valid
    public final QueryEditor queryEditor;

    @NotNull
    @Valid
    public final Common common;

    @NotNull
    @Valid
    public final History history;

    @NotNull
    @Valid
    public final FileStore fileStore;

    @NotNull
    @Valid
    public final Favourites favourites;

    @NotNull
    @Valid
    public final QueryExport queryExport;

    @NotNull
    @Valid
    public final HttpSession httpSession;

    @NotNull
    @Valid
    public final Cookies cookie;

    @NotNull
    @Valid
    public final QueryImport queryImport;

    @Inject
    public AppConfig(
	    Cassandra cassandra,
	    QueryEditor queryEditor,
	    Common common,
	    QueryExport queryExport,
	    Cookies cookie,
	    History history,
	    Favourites favourites,
	    FileStore fileStore,
	    HttpSession httpSession,
	    QueryImport queryImport) {
	this.cassandra = cassandra;
	this.queryEditor = queryEditor;
	this.common = common;
	this.queryExport = queryExport;
	this.cookie = cookie;
	this.history = history;
	this.favourites = favourites;
	this.fileStore = fileStore;
	this.httpSession = httpSession;
	this.queryImport = queryImport;
    }

    private static String crs(String cr, String str) throws UnsupportedEncodingException {
	String replaced = str.replaceAll("CR", String.valueOf((char) 10));
	return replaced;
    }

    public static AppConfig get() {
	Validate.notNull(instance, "Can not access AppConfig because spring initialization is not trough");
	return instance;
    }

    @PostConstruct
    @edu.umd.cs.findbugs.annotations.SuppressWarnings("ST_WRITE_TO_STATIC_FROM_INSTANCE_METHOD")
    void init() {
	instance = this;
	BeanValidator.create(this).validate();
	LOG.info("Read configuration: {}", this);
    }

    @Override
    public String toString() {
	return Objects.toStringHelper(this).add("cassandra", cassandra).add("queryEditor", queryEditor)
		.add("common", common).add("history", history).add("fileStore", fileStore)
		.add("favourites", favourites).add("queryExport", queryExport)
		.add("httpSession", httpSession).add("cookie", cookie).add("queryImport", queryImport)
		.toString();
    }

    @Named
    @Immutable
    public static class Cassandra implements Serializable {

	@Min(0)
	public final int port;

	@Min(0)
	public final int timeoutMillis;

	@Min(1)
	public final int columnsLimit;

	@NotEmpty
	public final String hosts;

	@Min(2)
	public final int maxConnectionsPerHost;

	@Min(1)
	public final int coreConnectionsPerHost;

	@Min(2)
	public final int maxSimultaneousRequestsPerConnectionThreshold;

	@Min(1)
	public final int minSimultaneousRequestsPerConnectionThreshold;

	public final boolean useSsl;

	@Inject
	public Cassandra(
		@Value("${cassandra.hosts}") String hosts,
		@Value("${cassandra.useSsl}") boolean useSsl,
		@Value("${cassandra.port}") int port,
		@Value("${cassandra.timeoutMillis}") int timeoutMillis,
		@Value("${cassandra.columnsLimit}") int columnsLimit,
		@Value("${cassandra.maxConnectionsPerHost}") int maxConnectionsPerHost,
		@Value("${cassandra.coreConnectionsPerHost}") int coreConnectionsPerHost,
		@Value("${cassandra.maxSimultaneousRequestsPerConnectionThreshold}") int maxSimultaneousRequestsPerConnectionThreshold,
		@Value("${cassandra.minSimultaneousRequestsPerConnectionThreshold}") int minSimultaneousRequestsPerConnectionThreshold) {
	    this.hosts = hosts;
	    this.useSsl = useSsl;
	    this.port = port;
	    this.timeoutMillis = timeoutMillis;
	    this.columnsLimit = columnsLimit;
	    this.maxConnectionsPerHost = maxConnectionsPerHost;
	    this.coreConnectionsPerHost = coreConnectionsPerHost;
	    this.maxSimultaneousRequestsPerConnectionThreshold = maxSimultaneousRequestsPerConnectionThreshold;
	    this.minSimultaneousRequestsPerConnectionThreshold = minSimultaneousRequestsPerConnectionThreshold;
	}

	@Override
	public String toString() {
	    return Objects
		    .toStringHelper(this)
		    .add("port", port)
		    .add("timeoutMillis", timeoutMillis)
		    .add("columnsLimit", columnsLimit)
		    .add("hosts", hosts)
		    .add("maxConnectionsPerHost", maxConnectionsPerHost)
		    .add("coreConnectionsPerHost", coreConnectionsPerHost)
		    .add(
			    "maxSimultaneousRequestsPerConnectionThreshold",
			    maxSimultaneousRequestsPerConnectionThreshold)
		    .add(
			    "minSimultaneousRequestsPerConnectionThreshold",
			    minSimultaneousRequestsPerConnectionThreshold).add("useSsl", useSsl).toString();
	}
    }

    @Named
    @Immutable
    public static class Common implements Serializable {
    }

    @Named
    @Immutable
    public static class Cookies implements Serializable {
	public final int expirySeconds;

	@Inject
	public Cookies(@Value("${cookies.expirySeconds}") int expirySeconds) {
	    this.expirySeconds = expirySeconds;
	}

	@Override
	public String toString() {
	    return "Cookies [expirySeconds=" + expirySeconds + "]";
	}

    }

    @Named
    @Immutable
    public static class QueryEditor implements Serializable {

	@Min(1)
	public final int maxColumnEmbeddedDisplayChars;

	@Min(1)
	public final int maxColumnDisplayChars;

	@Min(1)
	public final int maxColumnTooltipDisplayChars;

	@Min(1)
	public final int rowsLimit;

	@Inject
	protected QueryEditor(
		@Value("${queryEditor.maxColumnEmbeddedDisplayChars}") int maxColumnEmbeddedDisplayChars,
		@Value("${queryEditor.maxColumnDisplayChars}") int maxColumnDisplayChars,
		@Value("${queryEditor.maxColumnTooltipDisplayChars}") int maxColumnTooltipDisplayChars,
		@Value("${queryEditor.rowsLimit}") int rowsLimit

	) {
	    this.maxColumnEmbeddedDisplayChars = maxColumnEmbeddedDisplayChars;
	    this.maxColumnDisplayChars = maxColumnDisplayChars;
	    this.maxColumnTooltipDisplayChars = maxColumnTooltipDisplayChars;
	    this.rowsLimit = rowsLimit;
	}

	@Override
	public String toString() {
	    return Objects.toStringHelper(this)
		    .add("maxColumnEmbeddedDisplayChars", maxColumnEmbeddedDisplayChars)
		    .add("maxColumnDisplayChars", maxColumnDisplayChars).add("rowsLimit", rowsLimit)
		    .add("maxColumnTooltipDisplayChars", maxColumnTooltipDisplayChars).toString();
	}
    }

    @Named
    @Immutable
    public static final class QueryExport implements Serializable {

	@NotEmpty
	public final String querySeparator;

	@NotEmpty
	public final String rowSeparator;

	@NotEmpty
	public final String listSeparator;

	@NotEmpty
	public final String mapSeparator;

	@NotEmpty
	public final String columnSeparator;

	public final int crCharCode;

	@NotEmpty
	public final String valueBracketStart;

	@NotEmpty
	public final String fileName;

	@SimpleDate
	public final String fileNameDate;

	@NotEmpty
	public final String valueBracketEnd;

	@NotEmpty
	public final String encoding;

	public final boolean trim;

	public final boolean removeCrChars;

	@Inject
	public QueryExport(
		@Value("${queryExport.fileName}") String fileName,
		@Value("${queryExport.fileName.date}") String fileNameDate,
		@Value("${queryExport.querySeparator}") String querySeparator,
		@Value("${queryExport.rowSeparator}") String rowSeparator,
		@Value("${queryExport.columnSeparator}") String columnSeparator,
		@Value("${queryExport.listSeparator}") String listSeparator,
		@Value("${queryExport.mapSeparator}") String mapSeparator,
		@Value("${queryExport.valueBracketStart}") String valueBracketStart,
		@Value("${queryExport.valueBracketEnd}") String valueBracketEnd,
		@Value("${queryExport.crCharCode}") int crCharCode,
		@Value("${queryExport.removeCrChars}") boolean removeCrChars,
		@Value("${queryExport.trim}") boolean trim,
		@Value("${queryExport.encoding}") String encoding) throws UnsupportedEncodingException {
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
	    this.encoding = encoding;
	}

	@Override
	public String toString() {
	    return Objects.toStringHelper(this).add("querySeparator", querySeparator)
		    .add("rowSeparator", rowSeparator).add("listSeparator", listSeparator)
		    .add("mapSeparator", mapSeparator).add("columnSeparator", columnSeparator)
		    .add("crCharCode", crCharCode).add("valueBracketStart", valueBracketStart)
		    .add("fileName", fileName).add("fileNameDate", fileNameDate)
		    .add("valueBracketEnd", valueBracketEnd).add("trim", trim)
		    .add("removeCrChars", removeCrChars).toString();
	}
    }

    @Named
    @Immutable
    public static final class QueryImport implements Serializable {
	@NotNull
	public final Pattern listSeparatorRegEx;

	@NotEmpty
	public final String encoding;

	@Min(1)
	public final int maxFileSizeMb;

	@Min(1)
	public final int maxThreadsProImport;

	@Inject
	public QueryImport(
		@Value("${queryImport.listSeparatorRegEx}") String listSeparatorRegEx,
		@Value("${queryImport.encoding}") String encoding,
		@Value("${queryImport.maxFileSizeMb}") int maxFileSizeMb,
		@Value("${queryImport.parallel.maxThreadsProImport}") int maxThreadsProImport) {
	    try {
		this.listSeparatorRegEx = Pattern.compile(listSeparatorRegEx);
	    }
	    catch (PatternSyntaxException e) {
		throw new ServiceException("Property: queryImport.listSeparatorRegEx syntax error: "
			+ e.getMessage(), e);
	    }
	    this.encoding = encoding;
	    this.maxFileSizeMb = maxFileSizeMb;
	    this.maxThreadsProImport = maxThreadsProImport;
	}

	@Override
	public String toString() {
	    return Objects.toStringHelper(this).add("listSeparatorRegEx", listSeparatorRegEx)
		    .add("encoding", encoding).add("maxFileSizeMb", maxFileSizeMb)
		    .add("maxThreadsProImport", maxThreadsProImport).toString();
	}
    }

    @Named
    @Immutable
    public static class Favourites implements Serializable {
	public final int entriesLimit;

	public final boolean enabled;

	@Inject
	public Favourites(
		@Value("${favourites.entriesLimit:50}") int entriesLimit,
		@Value("${favourites.enabled:false}") boolean enabled) {
	    this.entriesLimit = entriesLimit;
	    this.enabled = enabled;
	}

	@Override
	public String toString() {
	    return Objects.toStringHelper(this).add("entriesLimit", entriesLimit).add("enabled", enabled)
		    .toString();
	}
    }

    @Named
    @Immutable
    public static class FileStore implements Serializable {

	public final int maxFileSize;

	public final int lockWaitTimeoutMillis;

	public final String folder;

	@Inject
	public FileStore(
		@Value("${fileStore.maxFileSize}") int maxFileSize,
		@Value("${fileStore.lockWaitTimeoutMillis}") int lockWaitTimeoutMillis,
		@Value("${fileStore.folder}") String folder) {
	    this.maxFileSize = maxFileSize;
	    this.lockWaitTimeoutMillis = lockWaitTimeoutMillis;
	    this.folder = folder;
	}

	@Override
	public String toString() {
	    return Objects.toStringHelper(this).add("maxFileSize", maxFileSize)
		    .add("lockWaitTimeoutMillis", lockWaitTimeoutMillis).add("folder", folder).toString();
	}
    }

    @Named
    @Immutable
    public static class History implements Serializable {
	public final int entriesLimit;

	public final boolean enabled;

	@Inject
	public History(
		@Value("${history.entriesLimit}") int entriesLimit,
		@Value("${history.enabled}") boolean enabled) {
	    this.entriesLimit = entriesLimit;
	    this.enabled = enabled;
	}

	@Override
	public String toString() {
	    return Objects.toStringHelper(this).add("entriesLimit", entriesLimit).add("enabled", enabled)
		    .toString();
	}
    }

    @Named
    @Immutable
    public static class HttpSession implements Serializable {
	public final int expirySeconds;

	@Inject
	public HttpSession(@Value("${httpSession.expirySeconds}") int expirySeconds) {
	    this.expirySeconds = expirySeconds;
	}

	@Override
	public String toString() {
	    return Objects.toStringHelper(this).add("expirySeconds", expirySeconds).toString();
	}
    }

}
