package org.cyclop.service.queryprotocoling.impl;

import net.jcip.annotations.NotThreadSafe;
import org.cyclop.model.QueryFavourites;
import org.cyclop.service.queryprotocoling.FavouritesService;
import org.springframework.context.annotation.Scope;
import org.springframework.context.annotation.ScopedProxyMode;
import org.springframework.validation.annotation.Validated;

import javax.inject.Named;

/** @author Maciej Miklas */
@NotThreadSafe
@Named
@Scope(value = "session", proxyMode = ScopedProxyMode.TARGET_CLASS)
@Validated
class FavouritesServiceImpl extends AbstractQueryProtocolingService<QueryFavourites> implements FavouritesService {

	@Override
	protected Class<QueryFavourites> getClazz() {
		return QueryFavourites.class;
	}

	@Override
	protected QueryFavourites createEmpty() {
		return new QueryFavourites();
	}

}
