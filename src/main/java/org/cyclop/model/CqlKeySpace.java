package org.cyclop.model;

import javax.annotation.concurrent.Immutable;

/**
 * @author Maciej Miklas
 */
@Immutable
public class CqlKeySpace extends CqlPart {

    public CqlKeySpace(String part) {
        super(part);
    }

    @Override
    public String toString() {
        return "CqlKeySpace{" + "part='" + part + '\'' + '}';
    }

    @Override
    public CqlType type() {
        return CqlType.KEYSPACE;
    }
}
