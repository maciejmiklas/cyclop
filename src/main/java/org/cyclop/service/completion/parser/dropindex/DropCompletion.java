package org.cyclop.service.completion.parser.dropindex;

import javax.inject.Named;
import org.cyclop.model.CqlKeyword;
import org.cyclop.service.completion.parser.NotSupportedCompletion;

/**
 * @author Maciej Miklas
 */
@Named("dropindex.DropCompletion")
public class DropCompletion extends NotSupportedCompletion {

    public DropCompletion() {
        super(CqlKeyword.Def.DROP.value);
    }

    @Override
    protected String getNotSupportedText() {
        return "drop index";
    }
}
