package org.cyclop.common;

import com.google.common.base.Objects;
import net.jcip.annotations.Immutable;
import org.cyclop.model.exception.ServiceException;
import org.cyclop.validation.SimpleDate;
import org.hibernate.validator.constraints.NotEmpty;
import org.springframework.beans.factory.annotation.Value;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;
import javax.validation.ConstraintViolation;
import javax.validation.Valid;
import javax.validation.Validator;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;
import java.io.UnsupportedEncodingException;
import java.util.Set;

/** @author Maciej Miklas */
@Named
public class AppConfig {

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
	public final CqlExport cqlExport;

	@NotNull
	@Valid
	public final Cookie cookie;

	@NotNull
	@Valid
	private static AppConfig instance = null;

	@Inject
	private Validator validator;

	public static AppConfig get() {
		if (instance == null) {
			throw new IllegalArgumentException("Can not access AppConfig because spring initialization is not trough");
		}
		return instance;
	}

	@PostConstruct
	@edu.umd.cs.findbugs.annotations.SuppressWarnings("ST_WRITE_TO_STATIC_FROM_INSTANCE_METHOD") void init() {
		instance = this;
		Set<ConstraintViolation<AppConfig>> validateRes = validator.validate(instance);
		if (!validateRes.isEmpty()) {
			throw new ServiceException("Application configuration file (cyclop.properties) contains errors: " + validateRes);
		}
	}

	private static String crs(String cr, String str) throws UnsupportedEncodingException {
		String replaced = str.replaceAll("CR", String.valueOf((char) 10));
		return replaced;
	}

	@Inject
	public AppConfig(Cassandra cassandra, CqlEditor cqlEditor, Common common, CqlExport cqlExport, Cookie cookie, History history) {
		this.cassandra = cassandra;
		this.cqlEditor = cqlEditor;
		this.common = common;
		this.cqlExport = cqlExport;
		this.cookie = cookie;
		this.history = history;
	}

	@Named
	@Immutable
	public static class Cassandra {

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

		public final boolean useSsl;

		@Inject
		public Cassandra(@Value("${cassandra.hosts}") String hosts, @Value("${cassandra.useSsl}") boolean useSsl,
						 @Value("${cassandra.port}") int port, @Value("${cassandra.timeoutMilis}") int timeoutMilis,
						 @Value("${cassandra.rowsLimit}") int rowsLimit,
						 @Value("${cassandra.columnsLimit}") int columnsLimit) {
			this.hosts = hosts;
			this.useSsl = useSsl;
			this.port = port;
			this.timeoutMilis = timeoutMilis;
			this.rowsLimit = rowsLimit;
			this.columnsLimit = columnsLimit;
		}

		@Override
		public String toString() {
			return Objects.toStringHelper(this).add("port", port).add("timeoutMilis", timeoutMilis).add("rowsLimit", rowsLimit)
					.add("columnsLimit", columnsLimit).add("hosts", hosts).add("useSsl", useSsl).toString();
		}
	}

	@Named
	@Immutable
	public static class History {
		public final int historyLimit;

		public final int starredLimit;

		public final boolean enabled;

		public final String folder;

		public final int maxFileSize;

		public final int lockWaitTimeoutMilis;

		@Inject
		public History(
				@Value("${history.historyLimit}") int historyLimit, @Value("${history.starredLimit}") int starredLimit,
				@Value("${history.folder}") String folder, @Value("${history.enabled}") boolean enabled,
				@Value("${history.maxFileSize}") int maxFileSize,
				@Value("${history.lockWaitTimeoutMilis}") int lockWaitTimeoutMilis) {
			this.historyLimit = historyLimit;
			this.starredLimit = starredLimit;
			this.folder = folder;
			this.enabled = enabled;
			this.maxFileSize = maxFileSize;
			this.lockWaitTimeoutMilis = lockWaitTimeoutMilis;
		}

	}

	@Named
	@Immutable
	public static class Common {
	}

	@Named
	@Immutable
	public static class Cookie {
		public final int expirySeconds;

		@Inject
		public Cookie(@Value("${cookie.expirySeconds}") int expirySeconds) {
			this.expirySeconds = expirySeconds;
		}
	}

	@Named
	@Immutable
	public static class CqlEditor {

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
			return Objects.toStringHelper(this).add("pageLimit", rowsPerPage)
					.add("maxColumnEmbeddedDisplayChars", maxColumnEmbeddedDisplayChars)
					.add("maxColumnDisplayChars", maxColumnDisplayChars)
					.add("maxColumnTooltipDisplayChars", maxColumnTooltipDisplayChars).toString();
		}
	}

	@Override
	public String toString() {
		return Objects.toStringHelper(this).add("cassandra", cassandra).add("cqlEditor", cqlEditor).toString();
	}

	@Named
	@Immutable
	public static final class CqlExport {

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
						 @Value("${cqlExport.trim}") boolean trim)
				throws UnsupportedEncodingException {
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
			return Objects.toStringHelper(this).add("querySeparator", querySeparator).add("rowSeparator", rowSeparator)
					.add("listSeparator", listSeparator).add("mapSeparator", mapSeparator).add("removeCrChars", removeCrChars)
					.toString();
		}
	}
}
