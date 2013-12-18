package org.cyclop.model;

/**
 * cql keywords are: create keyspace, use, alter keyspace, drop keyspace, create table, alter table, drop table,
 * truncate, create index, drop index, insert, update, delete, batch, select
 *
 * @author Maciej Miklas
 */
public class CqlKeyword extends CqlPart {

    public CqlKeyword(String part) {
        super(part);
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
