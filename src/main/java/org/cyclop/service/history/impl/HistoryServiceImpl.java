package org.cyclop.service.history.impl;

import javax.annotation.concurrent.NotThreadSafe;
import javax.inject.Inject;
import javax.inject.Named;
import org.cyclop.common.AppConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Scope;
import org.springframework.context.annotation.ScopedProxyMode;

/**
 * @author Maciej Miklas
 */
@NotThreadSafe
@Named
@Scope(value = "session", proxyMode = ScopedProxyMode.TARGET_CLASS)
public class HistoryServiceImpl /*implements HistoryService*/ {
    private final static Logger LOG = LoggerFactory.getLogger(HistoryServiceImpl.class);

    @Inject
    private AppConfig config;

}
