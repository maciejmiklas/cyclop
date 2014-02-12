package org.cyclop.web.pages.cqlcommander.export;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.behavior.AbstractAjaxBehavior;
import org.apache.wicket.request.handler.resource.ResourceStreamRequestHandler;
import org.apache.wicket.request.resource.ContentDisposition;
import org.apache.wicket.util.resource.IResourceStream;

/** @author Maciej Miklas */
abstract class DownloadBehavior extends AbstractAjaxBehavior {

	protected DownloadBehavior() {
	}

	public void initiateDownload(AjaxRequestTarget target) {
		String url = getCallbackUrl().toString();

		if (url.contains("?")) {
			url = url + "&antiCache=" + System.currentTimeMillis();
		} else {
			url = url + "?antiCache=" + System.currentTimeMillis();
		}

		target.appendJavaScript("setTimeout(\"window.location.href='" + url + "'\", 100);");
	}

	public void onRequest() {
		ResourceStreamRequestHandler handler = new ResourceStreamRequestHandler(getResourceStream(), getFileName());
		handler.setContentDisposition(ContentDisposition.ATTACHMENT);
		getComponent().getRequestCycle().scheduleRequestHandlerAfterCurrent(handler);
	}

	protected abstract String getFileName();

	protected abstract IResourceStream getResourceStream();
}
