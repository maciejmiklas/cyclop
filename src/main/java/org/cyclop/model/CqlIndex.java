package org.cyclop.model;

import javax.annotation.concurrent.Immutable;

/**
 * @author Maciej Miklas
 */
@Immutable
public final class CqlIndex extends CqlPart {

    public CqlIndex(String part) {
        super(part);
    }

    @Override
    public String toString() {
        return "CqlIndex{" + "part='" + part + '\'' + '}';
    }

    @Override
    public CqlType type() {
        return CqlType.INDEX;
    }
}
