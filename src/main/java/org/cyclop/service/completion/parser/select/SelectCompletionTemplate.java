package org.cyclop.service.completion.parser.select;

import java.util.ArrayList;
import java.util.List;
import javax.inject.Named;
import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlPart;
import org.cyclop.service.completion.parser.template.ColumnNameCompletionTemplate;

/**
 * @author Maciej Miklas
 */
@Named("select.SelectCompletionTemplate")
public class SelectCompletionTemplate extends ColumnNameCompletionTemplate {

    private final static List<CqlPart> staticPart = new ArrayList<>();

    public SelectCompletionTemplate() {
        super(staticPart, CqlKeyword.Def.FROM.value, CqlKeyword.Def.SELECT.value);
    }

    // TODO
    static {
        staticPart.add(CqlKeyword.Def.COUNT_AST.value);
        staticPart.add(CqlKeyword.Def.COUNT_ONE.value);
        staticPart.add(CqlKeyword.Def.WRITETIME.value);
        staticPart.add(CqlKeyword .Def.TTL.value);
        staticPart.add(CqlKeyword .Def.FROM.value);
    }

}
