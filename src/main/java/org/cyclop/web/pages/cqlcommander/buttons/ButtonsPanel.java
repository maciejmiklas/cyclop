package org.cyclop.web.pages.cqlcommander.buttons;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxFallbackLink;
import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.JavaScriptReferenceHeaderItem;
import org.apache.wicket.markup.head.OnDomReadyHeaderItem;
import org.apache.wicket.markup.html.link.Link;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.request.resource.JavaScriptResourceReference;
import org.cyclop.model.CqlQuery;
import org.cyclop.web.components.buttons.StateButton;

/** @author Maciej Miklas */
public class ButtonsPanel extends Panel {

	private static final JavaScriptResourceReference JS_EXPAND = new JavaScriptResourceReference(ButtonsPanel.class,
			"buttons.js");

	public ButtonsPanel(String id, final ButtonListener buttonListener, boolean completionPressed) {
		super(id);
		setRenderBodyOnly(true);

		AjaxFallbackLink<CqlQuery> addToFavourites = new AjaxFallbackLink<CqlQuery>("addToFavourites") {
			@Override
			public void onClick(AjaxRequestTarget target) {
				//buttonListener.onClickExecCql(target);
			}
		};
		add(addToFavourites);

		AjaxFallbackLink<CqlQuery> queryHistory = new AjaxFallbackLink<CqlQuery>("queryHistory") {
			@Override
			public void onClick(AjaxRequestTarget target) {
				//buttonListener.onClickExecCql(target);
			}
		};
		add(queryHistory);

		AjaxFallbackLink<CqlQuery> queryFavourites = new AjaxFallbackLink<CqlQuery>("queryFavourites") {
			@Override
			public void onClick(AjaxRequestTarget target) {
				//buttonListener.onClickExecCql(target);
			}
		};
		add(queryFavourites);

		AjaxFallbackLink<CqlQuery> execQuery = new AjaxFallbackLink<CqlQuery>("execQuery") {
			@Override
			public void onClick(AjaxRequestTarget target) {
				buttonListener.onClickExecCql(target);
				target.appendJavaScript("cqlQuerySuccessResponse()");
			}
		};
		add(execQuery);

		AjaxFallbackLink<CqlQuery> exportQueryResult = new AjaxFallbackLink<CqlQuery>("exportQueryResult") {
			@Override
			public void onClick(AjaxRequestTarget target) {
				buttonListener.onClickQueryResultExport(target);
			}
		};
		add(exportQueryResult);

		Link<CqlQuery> logOut = new Link<CqlQuery>("logOut") {

			@Override
			public void onClick() {
				buttonListener.onClickLogOut();
			}
		};
		add(logOut);

		AjaxFallbackLink<Void> completion = new StateButton("completion", completionPressed, "btn btn-sm btn-primary",
				"btn btn-sm btn-primary active") {
			@Override
			protected void onClick(AjaxRequestTarget target, boolean pressed) {
				buttonListener.onClickCompletion(target, pressed);
			}
		};
		add(completion);
	}

	@Override
	public void renderHead(IHeaderResponse response) {
		response.render(JavaScriptReferenceHeaderItem.forReference(JS_EXPAND));
		response.render(OnDomReadyHeaderItem.forScript("initButtons()"));
	}
}
