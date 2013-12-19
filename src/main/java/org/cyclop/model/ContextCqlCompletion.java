package org.cyclop.model;

import java.io.Serializable;
import java.util.Objects;

/**
 * @author Maciej Miklas
 */
public class ContextCqlCompletion implements Serializable {
    public final CqlQueryType queryType;

    public final CqlCompletion cqlCompletion;

    public ContextCqlCompletion(CqlQueryType queryType, CqlCompletion cqlCompletion) {
        if (queryType == null) {
            throw new IllegalArgumentException("Null queryType set");
        }

        if (cqlCompletion == null) {
            throw new IllegalArgumentException("Null cqlCompletion set");
        }
        this.queryType = queryType;
        this.cqlCompletion = cqlCompletion;
    }

    @Override
    public int hashCode() {
        return Objects.hash(queryType, cqlCompletion);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == null || getClass() != obj.getClass()) {
            return false;
        }
        final ContextCqlCompletion other = (ContextCqlCompletion) obj;
        return Objects.equals(queryType, other.queryType) && Objects.equals(cqlCompletion, other.cqlCompletion);
    }

    @Override
    public String toString() {
        return com.google.common.base.Objects.toStringHelper(this).add("queryType", queryType).add("cqlCompletion",
                cqlCompletion).toString();
    }
}
