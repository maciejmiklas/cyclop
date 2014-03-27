package org.cyclop.common;

import net.jcip.annotations.Immutable;
import org.cyclop.validation.BeanValidator;
import org.cyclop.validation.SimpleDate;
import org.hibernate.validator.constraints.NotEmpty;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;
import javax.validation.Valid;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.io.UnsupportedEncodingException;

/** @author Maciej Miklas */
@Named
public class AppConfig implements Serializable {
	private final static Logger LOG = LoggerFactory.getLogger(AppConfig.class);

	@NotNull
	@Valid
	private static AppConfig instance = null;

	@NotNull
	@Valid
	public final Cassandra cassandra;

	@NotNull
	@Valid
	public final CqlEditor cqlEditor;

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
	public final CqlExport cqlExport;

	@NotNull
	@Valid
	public final HttpSession httpSession;

	@NotNull
	@Valid
	public final Cookies cookie;

	@Inject
	public AppConfig(Cassandra cassandra, CqlEditor cqlEditor, Common common, CqlExport cqlExport, Cookies cookie,
					 History history, Favourites favourites, FileStore fileStore, HttpSession httpSession) {
		this.cassandra = cassandra;
		this.cqlEditor = cqlEditor;
		this.common = common;
		this.cqlExport = cqlExport;
		this.cookie = cookie;
		this.history = history;
		this.favourites = favourites;
		this.fileStore = fileStore;
		this.httpSession = httpSession;
	}

	public static AppConfig get() {
		if (instance == null) {
			throw new IllegalArgumentException("Can not access AppConfig because spring initialization is not trough");
		}
		return instance;
	}

	private static String crs(String cr, String str) throws UnsupportedEncodingException {
		String replaced = str.replaceAll("CR", String.valueOf((char) 10));
		return replaced;
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
		return "AppConfig [cassandra=" + cassandra + ", cqlEditor=" + cqlEditor + ", common=" + common + ", history=" +
				history + ", fileStore=" + fileStore + ", favourites=" + favourites + ", cqlExport=" + cqlExport +
				", httpSession=" + httpSession + ", cookie=" + cookie + "]";
	}

	@Named
	@Immutable
	public static class Cassandra implements Serializable {

		@Min(0)
		public final int port;

		@Min(0)
		public final int timeoutMilis;

		@Min(1)
		public final int rowsLimit;

		@Min(1)
		public final int columnsLimit;

		@NotEmpty
		public final String hosts;

		@Min(1)
		public final int maxConnectionsProSession;

		public final boolean useSsl;

		@Inject
		public Cassandra(@Value("${cassandra.hosts}") String hosts, @Value("${cassandra.useSsl}") boolean useSsl,
						 @Value("${cassandra.port}") int port, @Value("${cassandra.timeoutMilis}") int timeoutMilis,
						 @Value("${cassandra.rowsLimit}") int rowsLimit,
						 @Value("${cassandra.columnsLimit}") int columnsLimit,
						 @Value("${cassandra.maxConnectionsProSession}") int maxConnectionsProSession) {
			this.hosts = hosts;
			this.useSsl = useSsl;
			this.port = port;
			this.timeoutMilis = timeoutMilis;
			this.rowsLimit = rowsLimit;
			this.columnsLimit = columnsLimit;
			this.maxConnectionsProSession = maxConnectionsProSession;
		}

		@Override
		public String toString() {
			return "Cassandra [port=" + port + ", timeoutMilis=" + timeoutMilis + ", rowsLimit=" + rowsLimit +
					", columnsLimit=" + columnsLimit + ", hosts=" + hosts + ", maxConnectionsProSession=" +
					maxConnectionsProSession + ", useSsl=" + useSsl + "]";
		}

	}

	@Named
	@Immutable
	public static class FileStore implements Serializable {

		public final int maxFileSize;

		public final int lockWaitTimeoutMilis;

		public final String folder;

		@Inject
		public FileStore(@Value("${fileStore.maxFileSize}") int maxFileSize,
						 @Value("${fileStore.lockWaitTimeoutMilis}") int lockWaitTimeoutMilis,
						 @Value("${fileStore.folder}") String folder) {
			this.maxFileSize = maxFileSize;
			this.lockWaitTimeoutMilis = lockWaitTimeoutMilis;
			this.folder = folder;
		}

		@Override
		public String toString() {
			return "FileStore [maxFileSize=" + maxFileSize + ", lockWaitTimeoutMilis=" + lockWaitTimeoutMilis +
					", folder=" + folder + "]";
		}

	}

	@Named
	@Immutable
	public static class History implements Serializable {
		public final int entriesLimit;

		public final int queriesPerPage;

		public final boolean enabled;

		@Inject
		public History(@Value("${history.entriesLimit}") int entriesLimit, @Value("${history.enabled}") boolean enabled,
					   @Value("${history.queriesPerPage}") int queriesPerPage) {
			this.entriesLimit = entriesLimit;
			this.enabled = enabled;
			this.queriesPerPage = queriesPerPage;
		}

		@Override
		public String toString() {
			return "History [entriesLimit=" + entriesLimit + ", queriesPerPage=" + queriesPerPage + ", enabled=" +
					enabled + "]";
		}

	}

	@Named
	@Immutable
	public static class Favourites implements Serializable {
		public final int entriesLimit;

		public final boolean enabled;

		@Inject
		public Favourites(@Value("${favourites.entriesLimit}") int entriesLimit,
						  @Value("${favourites.enabled}") boolean enabled) {
			this.entriesLimit = entriesLimit;
			this.enabled = enabled;
		}

		@Override
		public String toString() {
			return "Favourites [entriesLimit=" + entriesLimit + ", enabled=" + enabled + "]";
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
	public static class HttpSession implements Serializable {
		public final int expirySeconds;

		@Inject
		public HttpSession(@Value("${httpSession.expirySeconds}") int expirySeconds) {
			this.expirySeconds = expirySeconds;
		}

		@Override
		public String toString() {
			return "HttpSession [expirySeconds=" + expirySeconds + "]";
		}

	}

	@Named
	@Immutable
	public static class CqlEditor implements Serializable {

		@Min(1)
		public final int rowsPerPage;

		@Min(1)
		public final int maxColumnEmbeddedDisplayChars;

		@Min(1)
		public final int maxColumnDisplayChars;

		@Min(1)
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
			return "CqlEditor [rowsPerPage=" + rowsPerPage + ", maxColumnEmbeddedDisplayChars=" +
					maxColumnEmbeddedDisplayChars + ", maxColumnDisplayChars=" + maxColumnDisplayChars +
					", maxColumnTooltipDisplayChars=" + maxColumnTooltipDisplayChars + "]";
		}

	}

	@Named
	@Immutable
	public static final class CqlExport implements Serializable {

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

		public final boolean trim;

		public final boolean removeCrChars;

		@Inject
		public CqlExport(@Value("${cqlExport.fileName}") String fileName,
						 @Value("${cqlExport.fileName.date}") String fileNameDate,
						 @Value("${cqlExport.querySeparator}") String querySeparator,
						 @Value("${cqlExport.rowSeparator}") String rowSeparator,
						 @Value("${cqlExport.columnSeparator}") String columnSeparator,
						 @Value("${cqlExport.listSeparator}") String listSeparator,
						 @Value("${cqlExport.mapSeparator}") String mapSeparator,
						 @Value("${cqlExport.valueBracketStart}") String valueBracketStart,
						 @Value("${cqlExport.valueBracketEnd}") String valueBracketEnd,
						 @Value("${cqlExport.crCharCode}") int crCharCode,
						 @Value("${cqlExport.removeCrChars}") boolean removeCrChars,
						 @Value("${cqlExport.trim}") boolean trim) throws UnsupportedEncodingException {
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
			return "CqlExport [querySeparator=" + querySeparator + ", rowSeparator=" + rowSeparator +
					", listSeparator=" + listSeparator + ", mapSeparator=" + mapSeparator + ", columnSeparator=" +
					columnSeparator + ", crCharCode=" + crCharCode + ", valueBracketStart=" + valueBracketStart +
					", fileName=" + fileName + ", fileNameDate=" + fileNameDate + ", valueBracketEnd=" +
					valueBracketEnd + ", trim=" + trim + ", removeCrChars=" + removeCrChars + "]";
		}

	}

}
