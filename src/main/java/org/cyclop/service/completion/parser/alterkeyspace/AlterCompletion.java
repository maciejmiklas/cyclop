package org.cyclop.service.completion.parser.alterkeyspace;

import com.google.common.base.Objects;
import javax.inject.Named;
import org.cyclop.model.CqlNotSupported;
import org.cyclop.service.completion.parser.NotSupportedCompletion;

/**
 * @author Maciej Miklas
 */
@Named("alterkeyspace.AlterCompletion")
public class AlterCompletion extends NotSupportedCompletion {

    public AlterCompletion() {
        super(new CqlNotSupported("alter"));
    }

    @Override
    protected String getNotSupportedText() {
        return "alter keyspace";
    }

    @Override
    public String toString() {
        return Objects.toStringHelper(this).toString();
    }
}
