package org.cyclop.model;

import com.google.common.base.Objects;
import net.jcip.annotations.Immutable;

/**
 * Cql keyword
 *
 * @author Maciej Miklas
 */
@Immutable
public final class CqlTable extends CqlPart {

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
