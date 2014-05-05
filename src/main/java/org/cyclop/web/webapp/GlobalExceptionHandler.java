package org.cyclop.web.webapp;

import org.apache.wicket.core.request.handler.PageProvider;
import org.apache.wicket.core.request.handler.RenderPageRequestHandler;
import org.apache.wicket.request.IRequestHandler;
import org.apache.wicket.request.cycle.AbstractRequestCycleListener;
import org.apache.wicket.request.cycle.RequestCycle;
import org.cyclop.model.exception.AuthenticationRequiredException;
import org.cyclop.web.pages.authenticate.AuthenticationPage;

/** @author Maciej Miklas */
public class GlobalExceptionHandler extends AbstractRequestCycleListener {

	@Override
	public IRequestHandler onException(RequestCycle cycle, Exception ex) {
		if (ex instanceof AuthenticationRequiredException) {
			return new RenderPageRequestHandler(new PageProvider(AuthenticationPage.class));
		}
		return super.onException(cycle, ex);
	}

}
