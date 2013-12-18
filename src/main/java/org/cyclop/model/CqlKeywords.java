package org.cyclop.model;

/**
 * @author Maciej Miklas
 */
public enum CqlKeywords {
    FROM("from"), DELETE("delete"), DROP("drop table"), INSERT("insert into"), UPDATE("update");

    private CqlKeywords(String value) {
        this.value = value;
        this.valueSp = value + " ";
    }

    public String value;

    public String valueSp;
}
