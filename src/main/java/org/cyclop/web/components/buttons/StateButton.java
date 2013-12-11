package org.cyclop.web.components.buttons;

import org.apache.wicket.AttributeModifier;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxFallbackLink;
import org.apache.wicket.model.IModel;

/**
 * @author Maciej Miklas
 */
public abstract class StateButton extends AjaxFallbackLink<Void> {

    private boolean pressed;

    public StateButton(final String id, final boolean initialPressed, final String cssEnabled,
                       final String cssPressed) {
        super(id);
        this.pressed = initialPressed;

        add(new AttributeModifier("class", new IModel<String>() {
            @Override
            public String getObject() {
                String css = pressed ? cssPressed : cssEnabled;
                return css;
            }

            @Override
            public void setObject(String object) {

            }

            @Override
            public void detach() {
            }
        }));
    }

    @Override
    public final void onClick(AjaxRequestTarget target) {
        target.add(this);
        pressed = !pressed;
        onClick(target, pressed);
    }

    protected abstract void onClick(AjaxRequestTarget target, boolean pressed);
}
