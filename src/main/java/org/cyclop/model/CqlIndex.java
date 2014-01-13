package org.cyclop.model;

/**
 * @author Maciej Miklas
 */
public class CqlIndex extends CqlPart {

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
