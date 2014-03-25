package org.cyclop.web.pages.error;

import org.apache.wicket.markup.html.basic.Label;
import org.cyclop.web.pages.parent.ParentPage;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.UUID;

public class ErrorPage extends ParentPage {
	private final static Logger LOG = LoggerFactory.getLogger(ErrorPage.class);

	public ErrorPage(Exception e) {
		String errorRef = UUID.randomUUID().toString();
		add(new Label("errorRef", errorRef));

		LOG.error("Got unhandled exception, Error ID: " + errorRef, e);
	}
}
