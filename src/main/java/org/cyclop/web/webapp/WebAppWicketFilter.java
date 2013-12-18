package org.cyclop.web.webapp;

import org.apache.wicket.protocol.http.WicketFilter;

import javax.servlet.annotation.WebFilter;
import javax.servlet.annotation.WebInitParam;

/**
 * @author Maciej Miklas
 */
@WebFilter(value = "/cyclop/*", initParams = {@WebInitParam(name = "applicationClassName",
        value = "org.cyclop.web.webapp.WicketWebApplication"), @WebInitParam(name = org.apache.wicket.protocol.http
        .WicketFilter.FILTER_MAPPING_PARAM, value = "/cyclop/*")})
public class WebAppWicketFilter extends WicketFilter {
}
