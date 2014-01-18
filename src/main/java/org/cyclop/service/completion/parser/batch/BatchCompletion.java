package org.cyclop.service.completion.parser.batch;

import com.google.common.base.Objects;
import javax.inject.Named;
import org.cyclop.model.CqlNotSupported;
import org.cyclop.service.completion.parser.NotSupportedCompletion;

/**
 * @author Maciej Miklas
 */
@Named("batch.BatchCompletion")
public class BatchCompletion extends NotSupportedCompletion {

    public BatchCompletion() {
        super(new CqlNotSupported("batch"));
    }

    @Override
    protected String getNotSupportedText() {
        return "batch";
    }

    @Override
    public String toString() {
        return Objects.toStringHelper(this).toString();
    }
}
