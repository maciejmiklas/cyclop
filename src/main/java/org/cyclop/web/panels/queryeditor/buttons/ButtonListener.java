package org.cyclop.web.panels.queryeditor.buttons;

import org.apache.wicket.ajax.AjaxRequestTarget;

import java.io.Serializable;

/** @author Maciej Miklas */
public interface ButtonListener extends Serializable {

	void onClickExecCql(AjaxRequestTarget target);

	void onClickQueryResultExport(AjaxRequestTarget target);

	void onClickCompletion(AjaxRequestTarget target, boolean pressed);
}
