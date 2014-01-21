package org.cyclop.model;

import java.io.Serializable;
import java.util.Objects;
import javax.annotation.concurrent.Immutable;

/**
 * @author Maciej Miklas
 */
@Immutable
public final class ContextCqlCompletion implements Serializable {
    public final CqlQueryName queryName;

    public final CqlCompletion cqlCompletion;

    public ContextCqlCompletion(CqlQueryName queryName, CqlCompletion cqlCompletion) {
        if (queryName == null) {
            throw new IllegalArgumentException("Null queryName set");
        }

        if (cqlCompletion == null) {
            throw new IllegalArgumentException("Null cqlCompletion set");
        }
        this.queryName = queryName;
        this.cqlCompletion = cqlCompletion;
    }

    @Override
    public int hashCode() {
        return Objects.hash(queryName, cqlCompletion);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == null || getClass() != obj.getClass()) {
            return false;
        }
        final ContextCqlCompletion other = (ContextCqlCompletion) obj;
        return Objects.equals(queryName, other.queryName) && Objects.equals(cqlCompletion, other.cqlCompletion);
    }

    @Override
    public String toString() {
        return com.google.common.base.Objects.toStringHelper(this).add("queryName", queryName).add("cqlCompletion",
                cqlCompletion).toString();
    }
}
