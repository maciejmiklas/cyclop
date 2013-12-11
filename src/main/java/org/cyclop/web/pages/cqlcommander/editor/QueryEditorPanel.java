package org.cyclop.web.pages.cqlcommander.editor;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.SortedSet;
import javax.inject.Inject;
import org.apache.commons.lang.StringUtils;
import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.attributes.AjaxRequestAttributes;
import org.apache.wicket.ajax.form.OnChangeAjaxBehavior;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.JavaScriptReferenceHeaderItem;
import org.apache.wicket.markup.head.OnDomReadyHeaderItem;
import org.apache.wicket.markup.html.form.TextArea;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.Model;
import org.apache.wicket.request.cycle.RequestCycle;
import org.cyclop.service.completion.CqlCompletionService;
import org.cyclop.service.model.ContextCqlCompletion;
import org.cyclop.service.model.CqlPart;
import org.cyclop.service.model.CqlQuery;
import org.cyclop.service.model.CqlQueryType;

import static org.cyclop.web.common.JsUtils.escapeParam;
import static org.cyclop.web.pages.parent.ScriptsRef.SUGGEST;

/**
 * @author Maciej Miklas
 */
public class QueryEditorPanel extends Panel {


    private final String editorMarkupIdJq;

    private final String editorMarkupIdJs;

    @Inject
    private CqlCompletionService cqlCompletionService;

    private ContextCqlCompletion currentCompletion;

    private final List<CompletionChangeListener> completionChangeListeners = new ArrayList<>();


    private TextArea<String> editor;

    public QueryEditorPanel(String id, String editorContent) {
        super(id);
        Injector.get().inject(this);

        editor = initEditor(StringUtils.trimToNull(editorContent));
        editorMarkupIdJq = "#" + editor.getMarkupId();
        editorMarkupIdJs = editor.getMarkupId();
    }

    public void registerCompletionChangeListener(CompletionChangeListener list) {
        completionChangeListeners.add(list);
    }

    public CqlQuery getEditorContent() {
        String editorValue = editor.getDefaultModelObjectAsString();
        editorValue = StringUtils.trimToNull(editorValue);
        if (editorValue == null) {
            return null;
        }
        CqlQuery cq = new CqlQuery(currentCompletion.queryType, editorValue);
        return cq;
    }


    private TextArea<String> initEditor(String editorContent) {
        final Model<String> editorModel = new Model<>();
        if (editorContent != null) {
            editorModel.setObject(editorContent);
        }

        TextArea<String> editor = new TextArea<>("queryEditor", editorModel);
        editor.setEscapeModelStrings(false);

        add(editor);

        editor.add(new OnChangeAjaxBehavior() {

            @Override
            protected void onUpdate(AjaxRequestTarget target) {

                Component cmp = getComponent();
                String editorValue = cmp.getDefaultModelObjectAsString();

                ContextCqlCompletion cqlCompletion;
                if (StringUtils.isEmpty(editorValue)) {
                    cqlCompletion = cqlCompletionService.findInitialCompletion();
                } else {
                    RequestCycle requestCycle = RequestCycle.get();
                    int index = requestCycle.getRequest().getRequestParameters().getParameterValue("cursorPos").toInt();
                    CqlQuery cqlQuery = new CqlQuery(CqlQueryType.UNKNOWN, editorValue);
                    cqlCompletion = cqlCompletionService.findCompletion(cqlQuery, index);
                }
                if (cqlCompletion.cqlCompletion.isEmpty() || cqlCompletion.equals(currentCompletion)) {
                    return;
                }

                fireCompletionChanged(cqlCompletion);
                String suggestsScript = generateReplaceSuggestsJs(editorMarkupIdJq,
                        cqlCompletion.cqlCompletion.fullCompletion);
                target.appendJavaScript(suggestsScript);

                for (CompletionChangeListener list : completionChangeListeners) {
                    Component refresh = list.getReferencesForRefresh();
                    if (refresh == null) {
                        continue;
                    }
                    target.add(refresh);
                }
            }

            @Override
            protected void updateAjaxAttributes(AjaxRequestAttributes attributes) {
                attributes.getDynamicExtraParameters().add("return {'cursorPos' : getCaretPosition(" +
                        editorMarkupIdJs + ")}");
                super.updateAjaxAttributes(attributes);
            }
        });
        return editor;
    }

    private void fireCompletionChanged(ContextCqlCompletion currentCompletion) {
        this.currentCompletion = currentCompletion;
        for (CompletionChangeListener list : completionChangeListeners) {
            list.onCompletionChange(currentCompletion);
        }
    }

    @Override
    public void renderHead(IHeaderResponse response) {
        response.render(JavaScriptReferenceHeaderItem.forReference(SUGGEST));

        ContextCqlCompletion cqlCompletion = cqlCompletionService.findInitialCompletion();
        fireCompletionChanged(cqlCompletion);

        String suggestsScript = generateInitSuggestsJs(editorMarkupIdJq, cqlCompletion.cqlCompletion.fullCompletion);
        response.render(OnDomReadyHeaderItem.forScript(suggestsScript));
    }

    private String generateInitSuggestsJs(String editorMarkupId, SortedSet<? extends CqlPart> suggestValues) {
        return generateSuggests("initSuggests", editorMarkupId, suggestValues);
    }

    private String generateReplaceSuggestsJs(String editorMarkupId, SortedSet<? extends CqlPart> suggestValues) {
        return generateSuggests("replaceSuggests", editorMarkupId, suggestValues);
    }

    private String generateSuggests(String function, String editorMarkupId,
                                    SortedSet<? extends CqlPart> suggestValues) {
        StringBuilder buf = new StringBuilder(function);
        buf.append("(");
        buf.append(escapeParam(editorMarkupId));
        buf.append(",[");

        Iterator<? extends CqlPart> suggestValuesIt = suggestValues.iterator();
        while (suggestValuesIt.hasNext()) {
            buf.append(escapeParam(suggestValuesIt.next().partLc));
            if (suggestValuesIt.hasNext()) {
                buf.append(",");
            }
        }
        buf.append("])");

        return buf.toString();
    }

}
