package org.cyclop.web.panels.queryeditor.buttons;

import org.apache.wicket.ajax.AjaxRequestTarget;

/** @author Maciej Miklas */
public interface ButtonListener {

	void onClickExecCql(AjaxRequestTarget target);

	void onClickQueryResultExport(AjaxRequestTarget target);

	void onQueryScriptImport(AjaxRequestTarget target);

	void onClickCompletion(AjaxRequestTarget target, boolean pressed);
}
