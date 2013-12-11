package org.cyclop.service.model;

/**
 *
 * @author Maciej Miklas
 */
public class CqlNotSupported extends CqlKeyword {

    public CqlNotSupported(String part) {
        super(part);
    }

    @Override
    public String toString() {
        return "CqlKeyword{" + "part='" + part + '\'' + '}';
    }

    @Override
    public CqlType type() {
        return CqlType.NOT_SUPPORTED;
    }
}
