package org.cyclop.web.pages.cqlcommander.buttons;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxFallbackLink;
import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.JavaScriptReferenceHeaderItem;
import org.apache.wicket.markup.head.OnDomReadyHeaderItem;
import org.apache.wicket.markup.html.link.Link;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.request.resource.JavaScriptResourceReference;
import org.cyclop.service.model.CqlQuery;
import org.cyclop.web.components.buttons.StateButton;

/**
 * @author Maciej Miklas
 */
public class ButtonsPanel extends Panel {

    private AjaxFallbackLink<Void> disableCompletionLink;
    private static final JavaScriptResourceReference JS_EXPAND = new JavaScriptResourceReference(ButtonsPanel.class,
            "buttons.js");

    public ButtonsPanel(String id, final ButtonListener buttonListener) {
        super(id);
        setRenderBodyOnly(true);

        AjaxFallbackLink <CqlQuery> executeQueryLink = new AjaxFallbackLink<CqlQuery>("execCql") {
            @Override
            public void onClick(AjaxRequestTarget target) {
                buttonListener.onClickExecCql(target);
                target.appendJavaScript("cqlQuerySuccessResponse()");
            }
        };
        add(executeQueryLink);

        AjaxFallbackLink <CqlQuery> exportCqlResult = new AjaxFallbackLink<CqlQuery>("exportCqlResult") {
            @Override
            public void onClick(AjaxRequestTarget target) {
                buttonListener.onClickQueryResultExport(target);
            }
        };
        add(exportCqlResult);

        Link<CqlQuery> logOut = new Link<CqlQuery>("logOut") {

            @Override
            public void onClick() {
                buttonListener.onClickLogOut();
            }
        };
        add(logOut);


        disableCompletionLink = new StateButton("disableCompletion", true, "btn btn-sm btn-primary","btn btn-sm btn-primary active"){
            @Override
            protected void onClick(AjaxRequestTarget target, boolean pressed) {
                buttonListener.onClickDisableCompletion(target, pressed);
            }
        };
        add(disableCompletionLink);
    }

    @Override
    public void renderHead(IHeaderResponse response) {
        response.render(JavaScriptReferenceHeaderItem.forReference(JS_EXPAND));
        response.render(OnDomReadyHeaderItem.forScript("initButtons()"));
    }
}
