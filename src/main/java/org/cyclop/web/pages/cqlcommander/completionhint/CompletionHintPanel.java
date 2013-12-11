package org.cyclop.web.pages.cqlcommander.completionhint;

import java.io.Serializable;
import java.util.Iterator;
import org.apache.wicket.AttributeModifier;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.cyclop.service.model.ContextCqlCompletion;
import org.cyclop.service.model.CqlPart;

/**
 * @author Maciej Miklas
 */
public class CompletionHintPanel extends Panel {

    private ContextCqlCompletion currentCompletion;
    private boolean lastCssSwitch;

    public CompletionHintPanel(String id, String headerText) {
        super(id);
        setOutputMarkupId(true);
        setOutputMarkupPlaceholderTag(true);

        WebMarkupContainer cqlInfoHint = new WebMarkupContainer("infoHint");
        cqlInfoHint.add(new AttributeModifier("class", new CqlInfoHintCssModel()));
        add(cqlInfoHint);

        Label cqlInfoHintContent = new Label("hintContent", new CqlInfoHintModel());
        cqlInfoHintContent.setEscapeModelStrings(false);
        cqlInfoHint.add(cqlInfoHintContent);

        Label headerTextContent = new Label("headerText", headerText);
        cqlInfoHint.add(headerTextContent);
    }

    public void changeCompletion(ContextCqlCompletion newCompletion) {
        if(currentCompletion != null && newCompletion != null && currentCompletion.equals(newCompletion)){
            return;
        }

        currentCompletion = newCompletion;
        lastCssSwitch = !lastCssSwitch;
    }

    private class CqlInfoHintCssModel implements IModel<String>, Serializable {

        @Override
        public String getObject() {

            String css = lastCssSwitch ? "cq-cqlInfoHint alert alert-dismissable cq-hintBodyA" : "cq-cqlInfoHint alert alert-dismissable cq-hintBodyB";
            return css;
        }

        @Override
        public void setObject(String object) {
        }

        @Override
        public void detach() {
        }
    }

    private class CqlInfoHintModel implements IModel<String>, Serializable {

        @Override
        public String getObject() {
            if (currentCompletion == null) {
                return ";-)";
            }

            StringBuilder buf = new StringBuilder();
            Iterator<? extends CqlPart> partIt = currentCompletion.cqlCompletion.minCompletion.iterator();
            while (partIt.hasNext()) {
                CqlPart part = partIt.next();

                String css;
                switch (part.type()) {
                    case KEYWORD:
                        css = "cq-hintCqlKeyword";
                        break;
                    case TABLE:
                        css = "cq-hintCqlTable";
                        break;
                    case COLUMN:
                        css = "cq-hintCqlColumn";
                        break;
                    case KEYSPACE:
                        css = "cq-hintCqlKeyspace";
                        break;
                    case NOT_SUPPORTED:
                        css = "cq-hintCqlNotSupported";
                        break;

                    default:
                        css = null;
                        break;
                }

                if (css == null) {
                    buf.append(part.part);
                } else {
                    buf.append("<span class=\"").append(css).append("\">").append(part.part);
                    if (partIt.hasNext()) {
                        buf.append(", ");
                    }
                    buf.append("</span>");
                }
            }

            return buf.toString();
        }

        @Override
        public void setObject(String object) {
        }

        @Override
        public void detach() {
        }
    }
}
