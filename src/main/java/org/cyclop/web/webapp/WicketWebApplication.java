package org.cyclop.web.webapp;

import org.apache.wicket.Page;
import org.apache.wicket.authroles.authentication.AbstractAuthenticatedWebSession;
import org.apache.wicket.authroles.authentication.AuthenticatedWebApplication;
import org.apache.wicket.markup.html.WebPage;
import org.apache.wicket.settings.IRequestCycleSettings;
import org.apache.wicket.spring.injection.annot.SpringComponentInjector;
import org.cyclop.web.pages.authenticate.AuthenticatePage;
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
		getComponentInstantiationListeners().add(new SpringComponentInjector(this));
		setPageManagerProvider(new NoSerializationPageManagerProvider(this));
		getMarkupSettings().setStripWicketTags(true);

		// disable page visioning
		getRequestCycleSettings().setRenderStrategy(IRequestCycleSettings.RenderStrategy.ONE_PASS_RENDER);

		initBookmarks();
	}

	private void initBookmarks() {
		mountPage("/ced", MainPage.class);
	}

	@Override
	protected Class<? extends AbstractAuthenticatedWebSession> getWebSessionClass() {
		return CqlWebSession.class;
	}

	@Override
	protected Class<? extends WebPage> getSignInPageClass() {
		return AuthenticatePage.class;
	}

}
