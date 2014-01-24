package org.cyclop.service.completion.impl.parser.createtable;

import com.google.common.base.Objects;
import javax.inject.Named;
import org.cyclop.model.CqlNotSupported;
import org.cyclop.service.completion.impl.parser.NotSupportedCompletion;

/**
 * @author Maciej Miklas
 */
@Named("createtable.CreateCompletion")
class CreateCompletion extends NotSupportedCompletion {

    public CreateCompletion() {
        super(new CqlNotSupported("create"));
    }

    @Override
    protected String getNotSupportedText() {
        return "create table";
    }

    @Override
    public String toString() {
        return Objects.toStringHelper(this).toString();
    }


}
