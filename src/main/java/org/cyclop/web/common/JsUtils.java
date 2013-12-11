package org.cyclop.web.common;

/**
 * @author Maciej Miklas
 */
public class JsUtils {

    public static String escapeParam(String param) {
        if (param == null) {
            return null;
        }
        return "\"" + param + "\"";
    }

    public static String escapeParamLower(String param) {
        if (param == null) {
            return null;
        }
        return escapeParam(param).toLowerCase();
    }

    public static String escapeParamUpper(String param) {
        if (param == null) {
            return null;
        }
        return escapeParam(param).toUpperCase();
    }
}
