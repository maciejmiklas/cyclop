package org.cyclop.model;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.cyclop.model.adapter.BooleanDefaultTrueAdapter;

import com.google.common.base.Objects;

/**
 * @author Maciej Miklas
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class UserPreferences {

    @XmlElement(name = "e_hi")
    @XmlJavaTypeAdapter(BooleanDefaultTrueAdapter.class)
    public boolean showCqlCompletionHint = true;

    // TODO not used yet
    @XmlElement(name = "e_he")
    @XmlJavaTypeAdapter(BooleanDefaultTrueAdapter.class)
    public boolean showCqlHelp = true;

    public boolean getShowCqlCompletionHint() {
        return showCqlCompletionHint;
    }

    public void setShowCqlCompletionHint(boolean showCqlCompletionHint) {
        this.showCqlCompletionHint = showCqlCompletionHint;
    }

    public boolean getShowCqlHelp() {
        return showCqlHelp;
    }

    public void setShowCqlHelp(boolean showCqlHelp) {
        this.showCqlHelp = showCqlHelp;
    }

    @Override
    public String toString() {
        return Objects.toStringHelper(this).add("showCqlCompletionHint", showCqlCompletionHint).add("showCqlHelp",
                showCqlHelp).toString();
    }


    @Override
    public int hashCode() {
        return java.util.Objects.hash(showCqlCompletionHint, showCqlHelp);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == null || getClass() != obj.getClass()) {
            return false;
        }
        final UserPreferences other = (UserPreferences) obj;
        return java.util.Objects.equals(showCqlCompletionHint, other.showCqlCompletionHint) && java.util.Objects
                .equals(showCqlHelp, other.showCqlHelp);
    }
}
