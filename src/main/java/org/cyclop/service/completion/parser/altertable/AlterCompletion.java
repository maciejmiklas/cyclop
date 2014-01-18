package org.cyclop.service.completion.parser.altertable;

import com.google.common.base.Objects;
import javax.inject.Named;
import org.cyclop.model.CqlNotSupported;
import org.cyclop.service.completion.parser.NotSupportedCompletion;

/**
 * @author Maciej Miklas
 */
@Named("altertable.AlterCompletion")
public class AlterCompletion extends NotSupportedCompletion {

    public AlterCompletion() {
        super(new CqlNotSupported("alter"));
    }

    @Override
    protected String getNotSupportedText() {
        return "alter table";
    }

    @Override
    public String toString() {
        return Objects.toStringHelper(this).toString();
    }
}
