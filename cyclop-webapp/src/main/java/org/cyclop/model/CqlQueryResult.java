package org.cyclop.model;

import com.datastax.driver.core.Row;
import com.google.common.base.Objects;

import org.apache.commons.collections4.iterators.EmptyIterator;
import org.cyclop.common.SerializationUtil;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.Serializable;
import java.util.Iterator;

/** @author Maciej Miklas */
public class CqlQueryResult implements Iterable<Row>, Serializable {
	public final static CqlQueryResult EMPTY = new CqlQueryResult();

	@NotNull
	@Valid
	public final CqlRowMetadata rowMetadata;

	@NotNull
	private final transient Iterator<Row> rows;

	@SuppressWarnings("unchecked")
	CqlQueryResult() {
		rows = EmptyIterator.INSTANCE;
		rowMetadata = CqlRowMetadata.EMPTY;
	}

	public CqlQueryResult(Iterator<Row> rowsIt, CqlRowMetadata rowMetadata) {
		this.rows = rowsIt;
		this.rowMetadata = rowMetadata;
	}

	@Override
	public Iterator<Row> iterator() {
		return rows;
	}

	private void readObject(ObjectInputStream in) throws ClassNotFoundException, IOException {
		in.defaultReadObject();
		SerializationUtil.setField(this, "rows", EmptyIterator.INSTANCE);
	}

	@Override
	public String toString() {
		return Objects.toStringHelper(this).add("rowMetadata", rowMetadata).toString();
	}

	public boolean isEmpty() {
		return !rows.hasNext();
	}
}
