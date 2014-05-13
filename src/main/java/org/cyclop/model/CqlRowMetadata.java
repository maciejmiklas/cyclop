package org.cyclop.model;

import com.google.common.collect.ImmutableList;
import net.jcip.annotations.Immutable;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import java.io.Serializable;

// TODO test serialization
/** @author Maciej Miklas */
@Immutable
public final class CqlRowMetadata implements Serializable {

	public final static CqlRowMetadata EMPTY = new CqlRowMetadata();

	/** List of columns that value is not empty for multiple rows */
	@NotNull
	@Valid
	public final ImmutableList<CqlExtendedColumnName> commonColumns;

	/** List of columns that value is not empty only for single row, or it's empty */
	@NotNull
	@Valid
	public final ImmutableList<CqlExtendedColumnName> dynamicColumns;

	/** all columns returned by db query in exact the same order */
	@NotNull
	@Valid
	public final ImmutableList<CqlExtendedColumnName> columns;

	/** could be null, it not found in result, or in case of error while reading meta data info */
	public final transient CqlPartitionKey partitionKey;

	public CqlRowMetadata(ImmutableList<CqlExtendedColumnName> commonColumns,
						  ImmutableList<CqlExtendedColumnName> dynamicColumns,
						  ImmutableList<CqlExtendedColumnName> columns,
						  CqlPartitionKey partitionKey) {
		this.commonColumns = commonColumns;
		this.dynamicColumns = dynamicColumns;
		this.columns = columns;
		this.partitionKey = partitionKey;
	}


	private CqlRowMetadata() {
		this.commonColumns = ImmutableList.of();
		this.dynamicColumns = commonColumns;
		this.columns = commonColumns;
		this.partitionKey = null;
	}
}
