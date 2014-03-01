package org.cyclop.model;

import com.datastax.driver.core.Row;
import com.google.common.base.Objects;
import com.google.common.collect.ImmutableList;
import net.jcip.annotations.Immutable;
import org.cyclop.common.SerializationUtil;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.Serializable;

/** @author Maciej Miklas */
@Immutable
public final class CqlSelectResult implements Serializable {

	/** List of columns that can be found in every row returned by the query */
	@NotNull @Valid
	public final ImmutableList<CqlExtendedColumnName> commonColumns;

	/** List of columns that can be found is particular rows */
	@NotNull @Valid
	public final ImmutableList<CqlExtendedColumnName> dynamicColumns;

	@NotNull @Valid
	public final transient ImmutableList<Row> rows;

	/** could be null, it not found in result, or in case of error while reading meta data info */
	public final transient CqlPartitionKey partitionKey;

	public CqlSelectResult() {
		this.commonColumns = ImmutableList.of();
		this.dynamicColumns = ImmutableList.of();
		this.rows = ImmutableList.of();
		this.partitionKey = null;
	}

	private void readObject(ObjectInputStream in) throws ClassNotFoundException, IOException {
		in.defaultReadObject();
		SerializationUtil.setFiled(this, "rows", ImmutableList.of());
	}

	public CqlSelectResult(ImmutableList<CqlExtendedColumnName> commonColumns,
						   ImmutableList<CqlExtendedColumnName> dynamicColumns, ImmutableList<Row> rows,
						   CqlPartitionKey partitionKey) {
		this.commonColumns = commonColumns;
		this.dynamicColumns = dynamicColumns;
		this.rows = rows;
		this.partitionKey = partitionKey;
	}

	public boolean isEmpty() {
		return rows == null || rows.isEmpty();
	}

	@Override
	public String toString() {
		return Objects.toStringHelper(this).add("commonColumns", commonColumns).add("dynamicColumns", dynamicColumns)
				.add("rows", rows).add("partitionKey", partitionKey).toString();
	}
}
