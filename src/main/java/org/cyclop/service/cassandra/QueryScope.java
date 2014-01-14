package org.cyclop.service.cassandra;

import javax.inject.Named;
import org.cyclop.model.CqlKeySpace;
import org.springframework.context.annotation.Scope;
import org.springframework.context.annotation.ScopedProxyMode;

/**
 * @author Maciej Miklas
 */
@Named
@Scope(value = "session", proxyMode = ScopedProxyMode.TARGET_CLASS)
public class QueryScope {

    private CqlKeySpace activeKeySpace;

    public CqlKeySpace getActiveKeySpace() {
        return activeKeySpace;
    }

    protected void setActiveKeySpace(CqlKeySpace activeSpace) {
        this.activeKeySpace = activeSpace;
    }
}
