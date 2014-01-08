package org.cyclop.model;

/**
 * cql keywords are: create keyspace, use, alter keyspace, drop keyspace, create table, alter table, drop table,
 * truncate, create index, drop index, insert, update, delete, batch, select
 *
 * @author Maciej Miklas
 */
public class CqlKeyword extends CqlPart {

    public final String valueSp;

    public static enum Def {
        FROM("from"), DELETE("delete"), DROP("drop table"), INSERT_INTO("insert into"), INSERT("insert"),
        UPDATE("update"), TRUNCATE("truncate"),
        WHERE("where"), USING_TIMESTAMP("using  timestamp"), USING_TTL("using ttl"), ORDER_BY("order by"),
        ASC("asc"), DESC("desc"), WITH("with"),
        LIMIT("limit"), ALLOW_FILTERING("allow filtering"), TOKEN("token"), IN("in"), AND("and"), IN_BL("in ("),
        DROP_TABLE("drop table"), VALUES("values"), SELECT("select"), COUNT_AST("count (*)"), COUNT_ONE("count (1)"),
        WRITETIME("writetime"), TTL("ttl"), SET("set"), USE("use"),
        DROP_KEYSPACE("drop keyspace"), CREATE_KEYSPACE("create keyspace"), REPLICATION("replication"),
        IF_NOT_EXISTS("if not exists"),
        CLASS("class"), SIMPLE_STRATEGY("simplestrategy"), REPLICATION_FACTOR("replication_factor"),
        NETWORK_TOPOLOGY_STRATEGY("networktopologystrategy"), DURABLE_WRITES("durable_writes"), TRUE("true"),
        FALSE("false"),
        OLD_NETWORK_TOPOLOGY_STRATEGY("OldNetworkTopologyStrategy");

        private Def(String value) {
            this.value = new CqlKeyword(value.toLowerCase());
        }

        public CqlKeyword value;
    }

    protected CqlKeyword(String val) {
        super(val);
        this.valueSp = val.toLowerCase() + " ";
    }

    @Override
    public String toString() {
        return "CqlKeyword{" + "part='" + part + '\'' + '}';
    }

    @Override
    public CqlType type() {
        return CqlType.KEYWORD;
    }
}
