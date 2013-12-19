package org.cyclop.service.completion.parser.createkeyspace;

import javax.inject.Named;
import org.cyclop.model.CqlNotSupported;
import org.cyclop.service.completion.parser.NotSupportedCompletion;

/**
 * @author Maciej Miklas
 */
@Named("createkeyspace.CreateCompletion")
public class CreateCompletion extends NotSupportedCompletion {

    public CreateCompletion() {
        super(new CqlNotSupported("create"));
    }

    @Override
    protected String getNotSupportedText() {
        return "create keyspace";
    }
}
