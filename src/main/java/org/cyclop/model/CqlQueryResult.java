package org.cyclop.model;

import com.datastax.driver.core.Row;
import com.google.common.base.Objects;
import com.google.common.collect.ImmutableList;
import net.jcip.annotations.Immutable;
import net.jcip.annotations.NotThreadSafe;
import org.cyclop.common.SerializationUtil;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import javax.xml.bind.annotation.XmlTransient;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.Serializable;
import java.util.Iterator;

/** @author Maciej Miklas */
@Immutable
public final class CqlQueryResult implements Serializable, Iterable<Row> {

	/** List of columns that can be found in every row returned by the query */
	@NotNull
	@Valid
	public final ImmutableList<CqlExtendedColumnName> commonColumns;

	/** List of columns that can be found is particular rows */
	@NotNull
	@Valid
	public final ImmutableList<CqlExtendedColumnName> dynamicColumns;

	/** could be null, it not found in result, or in case of error while reading meta data info */
	public final transient CqlPartitionKey partitionKey;

	public final int rowsSize;

	@NotNull
	@Valid
	private final transient ImmutableList<Row> rows;

	public CqlQueryResult() {
		this.commonColumns = ImmutableList.of();
		this.dynamicColumns = ImmutableList.of();
		this.rows = ImmutableList.of();
		this.partitionKey = null;
		this.rowsSize = -1;
	}

	public CqlQueryResult(ImmutableList<CqlExtendedColumnName> commonColumns,
						  ImmutableList<CqlExtendedColumnName> dynamicColumns, ImmutableList<Row> rows,
						  CqlPartitionKey partitionKey) {
		this.commonColumns = commonColumns;
		this.dynamicColumns = dynamicColumns;
		this.rows = rows;
		this.partitionKey = partitionKey;
		this.rowsSize = rows.size();
	}

	private void readObject(ObjectInputStream in) throws ClassNotFoundException, IOException {
		in.defaultReadObject();
		SerializationUtil.setFiled(this, "rows", ImmutableList.of());
		SerializationUtil.setFiled(this, "rowsSize", 0);
	}

	public boolean isEmpty() {
		return rows == null || rows.isEmpty();
	}

	@Override
	public RowIterator iterator() {
		return new RowIterator(rows);
	}

	// TODO !!!
	// test
	public RowIterator iterator(int first, int count) {
		return new RowIterator(rows.subList(first, first + count));
	}

	@Override
	public String toString() {
		return Objects.toStringHelper(this).add("commonColumns", commonColumns).add("dynamicColumns", dynamicColumns)
				.add("rows", rows).add("partitionKey", partitionKey).toString();
	}

	/**
	 * CALL CLOSE ON ITERATOR BECAUSE IT HOLDS READ-LOCK <br> Iterates over history entries from newest to oldest entry
	 * (reversed fifo col)
	 */
	@XmlTransient
	@NotThreadSafe
	public final static class RowIterator implements Iterator<Row> {

		private final Iterator<Row> rowsIt;

		RowIterator(ImmutableList<Row> rows) {
			rowsIt = rows.iterator();
		}

		@Override
		public boolean hasNext() {
			return rowsIt.hasNext();
		}

		@Override
		public Row next() {
			return rowsIt.next();
		}

		@Override
		public void remove() {
			throw new UnsupportedOperationException("Remove is not supported on RowIterator");
		}
	}
}
