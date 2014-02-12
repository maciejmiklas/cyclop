package org.cyclop.web.pages.cqlcommander.cqlhelp;

import com.google.common.base.Charsets;
import com.google.common.io.Resources;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.cyclop.model.ContextCqlCompletion;

import java.io.IOException;
import java.net.URL;

/** @author Maciej Miklas */
public class CqlHelpPanel extends Panel {

	private Label cqlHelpContent;

	private ContextCqlCompletion currentCompletion;

	public CqlHelpPanel(String id) {
		super(id);
		setOutputMarkupId(true);
		cqlHelpContent = new Label("helpContent", new CqlHelpContentModel());
		cqlHelpContent.setEscapeModelStrings(false);
		add(cqlHelpContent);
	}

	public void changeCompletion(ContextCqlCompletion currentCompletion) {
		this.currentCompletion = currentCompletion;
	}

	private class CqlHelpContentModel implements IModel<String> {

		@Override
		public String getObject() {
			if (currentCompletion == null) {
				return ";-)";
			}

			String name = "help_" + currentCompletion.queryName.name().toLowerCase() + ".html";
			try {

				URL url = Resources.getResource("/org/cyclop/web/pages/cqlcommander/cqlhelp/help/" + name);

				String text = Resources.toString(url, Charsets.UTF_8);
				return text;
			} catch (IOException | IllegalArgumentException e) {
				return "Help file:'" + name + "' found :-(";
			}
		}

		@Override
		public void setObject(String object) {
		}

		@Override
		public void detach() {
		}
	}
}
