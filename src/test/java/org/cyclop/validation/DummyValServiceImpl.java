package org.cyclop.validation;

import org.cyclop.model.CqlKeySpace;
import org.cyclop.model.CqlTable;
import org.cyclop.model.exception.ServiceException;

import javax.inject.Named;

import static org.junit.Assert.assertNotNull;

@EnableValidation
@Named
public class DummyValServiceImpl implements DummyValService {

	@Override
	public String singleStringParam(String a) {
		assertNotNull(a);
		return a;
	}

	@Override
	public CqlKeySpace mixedParams(CqlKeySpace space, CqlTable table, String param) {
		assertNotNull(param);
		return space;
	}

	@Override
	public void singleParam(CqlTable table) {

	}

	@Override
	public void singleParamThrows(CqlTable table) throws Exception {
		if (table.partLc.equals("throw")) {
			throw new ServiceException("ex abc");
		}
	}

	@Override
	public CqlKeySpace returnOptional(CqlKeySpace space) {
		return space;
	}

	@Override
	public CqlKeySpace novalidation(CqlKeySpace space) {
		return space;
	}

}
