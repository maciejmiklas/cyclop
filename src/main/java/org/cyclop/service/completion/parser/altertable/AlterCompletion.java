package org.cyclop.service.completion.parser.altertable;

import org.cyclop.model.CqlNotSupported;
import org.cyclop.model.CqlPart;
import org.cyclop.service.completion.parser.NotSupportedCompletion;

import javax.inject.Named;

/**
 * @author Maciej Miklas
 */
@Named("altertable.AlterCompletion")
public class AlterCompletion extends NotSupportedCompletion {

    private final static CqlPart SM = new CqlNotSupported("alter");

    @Override
    public CqlPart startMarker() {
        return SM;
    }

    @Override
    protected String getNotSupportedText() {
        return "alter table";
    }
}
