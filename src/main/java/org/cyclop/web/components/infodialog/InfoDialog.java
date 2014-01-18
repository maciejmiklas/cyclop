package org.cyclop.web.components.infodialog;

import org.apache.wicket.AttributeModifier;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.util.template.PackageTextTemplate;
import org.cyclop.web.common.JsTextTemplate;

import java.util.HashMap;
import java.util.Map;

/**
 * @author Maciej Miklas
 */
public class InfoDialog extends Panel {

    private JsTextTemplate infoJs = new JsTextTemplate(new PackageTextTemplate(InfoDialog.class, "infoDialog.js"));

    private final WebMarkupContainer infoDialog;

    private final StringModel titleModel;

    private final StringModel messageModel;

    public InfoDialog(String id) {
        super(id);
        infoDialog = new WebMarkupContainer("infoDialog");
        infoDialog.setVisible(false);
        infoDialog.setOutputMarkupId(true);
        infoDialog.setOutputMarkupPlaceholderTag(true);
        titleModel = new StringModel();
        infoDialog.add(new AttributeModifier("title", titleModel));

        add(infoDialog);

        messageModel = new StringModel();
        Label message = new Label("message", messageModel);
        infoDialog.add(message);
    }

    public void open(AjaxRequestTarget target, String linkNameToDisable, String title, String message) {
        titleModel.setObject(title);
        messageModel.setObject(message);
        infoDialog.setVisible(true);
        target.add(infoDialog);

        Map<String, String> jsVariables = new HashMap<>();
        jsVariables.put("infoDialogId", "#" + infoDialog.getMarkupId());
        jsVariables.put("linkNameToDisable", "#" + linkNameToDisable);
        String jsContent = infoJs.asString(jsVariables);
        target.appendJavaScript(jsContent);
    }

    private final class StringModel implements IModel<String> {

        private String value = "";

        @Override
        public String getObject() {
            return value;
        }

        @Override
        public void setObject(String object) {
            this.value = object;
        }

        @Override
        public void detach() {
        }

    }
}
