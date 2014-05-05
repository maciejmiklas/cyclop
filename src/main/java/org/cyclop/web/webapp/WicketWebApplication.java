package org.cyclop.web.webapp;

import org.apache.wicket.Page;
import org.apache.wicket.authroles.authentication.AbstractAuthenticatedWebSession;
import org.apache.wicket.authroles.authentication.AuthenticatedWebApplication;
import org.apache.wicket.core.request.handler.PageProvider;
import org.apache.wicket.core.request.handler.RenderPageRequestHandler;
import org.apache.wicket.markup.html.SecurePackageResourceGuard;
import org.apache.wicket.markup.html.WebPage;
import org.apache.wicket.request.IRequestHandler;
import org.apache.wicket.request.cycle.AbstractRequestCycleListener;
import org.apache.wicket.request.cycle.RequestCycle;
import org.apache.wicket.settings.IRequestCycleSettings;
import org.apache.wicket.spring.injection.annot.SpringComponentInjector;
import org.cyclop.web.pages.authenticate.AuthenticationPage;
import org.cyclop.web.pages.error.ErrorPage;
import org.cyclop.web.pages.main.MainPage;

/** @author Maciej Miklas */
public class WicketWebApplication extends AuthenticatedWebApplication {

	@Override
	public Class<? extends Page> getHomePage() {
		return MainPage.class;
	}

	@Override
	protected void init() {
		super.init();

		setupWicket();
		setupSpring();
		setupBookmarks();
		setupSecurity();
		setupExceptionHandler();
		setupErrorPage();
	}

	private void setupErrorPage() {
		getRequestCycleListeners().add(new AbstractRequestCycleListener() {
			@Override
			public IRequestHandler onException(RequestCycle cycle, Exception ex) {
				return new RenderPageRequestHandler(new PageProvider(new ErrorPage(ex)));
			}
		});
	}

	private void setupExceptionHandler() {
		// show login page on session timeout
		getApplicationSettings().setPageExpiredErrorPage(AuthenticationPage.class);

		getRequestCycleListeners().add(new GlobalExceptionHandler());
	}

	private void setupSpring() {
		getComponentInstantiationListeners().add(new SpringComponentInjector(this));
	}

	private void setupWicket() {
		getMarkupSettings().setStripWicketTags(true);
		getMarkupSettings().setDefaultMarkupEncoding("UTF-8");
		getRequestCycleSettings().setResponseRequestEncoding("UTF-8");
		setPageManagerProvider(new NoSerializationPageManagerProvider(this));
		getRequestCycleSettings().setRenderStrategy(IRequestCycleSettings.RenderStrategy.ONE_PASS_RENDER);
	}

	private void setupBookmarks() {
		mountPage("/ced", MainPage.class);
	}

	private void setupSecurity() {
		SecurePackageResourceGuard guard = (SecurePackageResourceGuard) getResourceSettings().getPackageResourceGuard();
		guard.addPattern("+*.map");
	}

	@Override
	protected Class<? extends AbstractAuthenticatedWebSession> getWebSessionClass() {
		return CqlWebSession.class;
	}

	@Override
	protected Class<? extends WebPage> getSignInPageClass() {
		return AuthenticationPage.class;
	}

}
