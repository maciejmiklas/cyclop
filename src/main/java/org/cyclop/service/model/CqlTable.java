package org.cyclop.service.model;

import com.google.common.base.Objects;

/**
 * Cql keyword
 *
 * @author Maciej Miklas
 */
public class CqlTable extends CqlPart {

    /**
     * can be null
     */
    public final CqlKeySpace keySpace;

    public CqlTable(String table, String keySpace) {
        super(table);

        if (keySpace == null) {
            this.keySpace = null;
        } else {
            this.keySpace = new CqlKeySpace(keySpace);
        }
    }

    public CqlTable(String table) {
        this(table, null);
    }

    @Override
    public String toString() {
        return Objects.toStringHelper(this).add("table", part).add("keySpace", keySpace).toString();
    }

    @Override
    public CqlType type() {
        return CqlType.TABLE;
    }
}
