package org.cyclop.model;

import java.util.concurrent.locks.Lock;

/**
 * Some POJOs are synchronized due to concurrent access. Reflection can access
 * fields of those objects directly bypassing serialization. In this case
 * reflection code is being synchronized on lock returned by getLock();
 * 
 * @author Maciej Miklas
 */
public interface Synchronizable {

    Lock getLock();

}
