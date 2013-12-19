package org.cyclop.service.completion.parser.truncate;

import javax.inject.Named;
import org.cyclop.model.CqlPart;
import org.cyclop.service.completion.parser.NotSupportedCompletion;

/**
 * @author Maciej Miklas
 */
@Named("truncate.TruncateCompletion")
public class TruncateCompletion extends NotSupportedCompletion {

    public TruncateCompletion() {
        super(new CqlPart("truncate"));
    }

    @Override
    protected String getNotSupportedText() {
        return "truncate";
    }
}
