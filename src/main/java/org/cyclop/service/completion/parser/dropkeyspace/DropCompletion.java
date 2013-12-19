package org.cyclop.service.completion.parser.dropkeyspace;

import javax.inject.Named;
import org.cyclop.model.CqlKeyword;
import org.cyclop.service.completion.parser.NotSupportedCompletion;

/**
 * @author Maciej Miklas
 */
@Named("dropkeyspace.DropCompletion")
public class DropCompletion extends NotSupportedCompletion {

    public DropCompletion() {
        super(CqlKeyword.Def.DROP.value);
    }

    @Override
    protected String getNotSupportedText() {
        return "drop keyspace";
    }
}
