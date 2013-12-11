package org.cyclop.web.common;

import java.util.Map;
import org.apache.wicket.util.template.TextTemplate;
import org.apache.wicket.util.template.TextTemplateDecorator;

/**
 * @author Maciej Miklas
 */
public class JsTextTemplate extends TextTemplateDecorator {

    public JsTextTemplate(TextTemplate textTemplate) {
        super(textTemplate);
    }

    @Override
    public String getBeforeTemplateContents() {
        return "";
    }

    @Override
    public String getAfterTemplateContents() {
        return "";
    }

    @Override
    public TextTemplate interpolate(Map<String, ?> variables) {
        return this;
    }

}
