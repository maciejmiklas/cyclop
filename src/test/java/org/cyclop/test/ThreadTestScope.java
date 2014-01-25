package org.cyclop.test;

import org.springframework.beans.factory.ObjectFactory;
import org.springframework.context.support.SimpleThreadScope;

import javax.inject.Named;
import java.util.HashMap;
import java.util.Map;

/**
 * @author Maciej Miklas
 */
@Named
public class ThreadTestScope extends SimpleThreadScope {

    private Map<String, Object> map = new HashMap<>();

    private boolean singleThread = false;

    public synchronized void setSingleThread(boolean singleThread) {
        this.singleThread = singleThread;
    }

    @Override
    public synchronized Object get(String name, ObjectFactory objectFactory) {
        if (singleThread) {
            Object object = map.get(name);
            if (object == null) {
                object = objectFactory.getObject();
                map.put(name, object);
            }
            return object;
        } else {
            return super.get(name, objectFactory);
        }
    }

    @Override
    public synchronized Object remove(String s) {
        if (singleThread) {
            return map.remove(s);
        } else {
            return super.remove(s);
        }
    }

    @Override
    public void registerDestructionCallback(String s, Runnable runnable) {
    }

    @Override
    public Object resolveContextualObject(String s) {
        return null;
    }

    @Override
    public String getConversationId() {
        return Thread.currentThread().getName();
    }

}
