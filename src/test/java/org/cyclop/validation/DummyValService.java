package org.cyclop.validation;

import org.cyclop.model.CqlKeySpace;
import org.cyclop.model.CqlTable;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;

public interface DummyValService {

	@NotNull
	String singleStringParam(@NotNull String a);

	@NotNull
	CqlKeySpace mixedParams(@Valid CqlKeySpace space, @Valid CqlTable table, @NotNull String param);

	@Valid
	CqlKeySpace returnOptional(CqlKeySpace space);

	CqlKeySpace novalidation(CqlKeySpace space);

	void singleParam(@Valid CqlTable table);

	void singleParamThrows(@NotNull CqlTable table) throws Exception;
}
