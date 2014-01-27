package org.cyclop.service.completion.impl.parser.insert;

import com.google.common.base.Objects;
import net.jcip.annotations.ThreadSafe;
import javax.inject.Named;
import org.cyclop.model.CqlCompletion;
import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlPart;
import org.cyclop.service.completion.impl.parser.template.StaticMarkerBasedCompletion;

/**
 * @author Maciej Miklas
 */
@Named("insert.AfterColumnsCompletion")
@ThreadSafe
class AfterColumnsCompletion extends StaticMarkerBasedCompletion {

    public AfterColumnsCompletion() {
        super(new CqlPart(")"), CqlCompletion.Builder.naturalOrder().all(CqlKeyword.Def.VALUES.value).build());
    }
    @Override
    public String toString() {
        return Objects.toStringHelper(this).toString();
    }
}
