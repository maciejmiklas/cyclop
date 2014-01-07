package org.cyclop.service.completion.parser.insert;

import javax.inject.Named;
import org.cyclop.model.CqlCompletion;
import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlPart;
import org.cyclop.service.completion.parser.template.StaticMarkerBasedCompletion;

/**
 * @author Maciej Miklas
 */
@Named("insert.AfterColumnsCompletion")
public class AfterColumnsCompletion extends StaticMarkerBasedCompletion {

    public AfterColumnsCompletion() {
        super(new CqlPart(")"), CqlCompletion.Builder.naturalOrder().all(CqlKeyword.Def.VALUES.value).build());
    }

}
