package org.cyclop.model;

import org.cyclop.model.exception.ServiceException;

import java.lang.reflect.Field;

public class SerializationUtil {

	public static void setFiled(Object obj, String fieldName, Object val) {
		try {
			Class<?> clazz = obj.getClass();
			Field f = clazz.getDeclaredField("myFinalField");
			f.setAccessible(true);
			f.set(obj, val);
		} catch (NoSuchFieldException | SecurityException | IllegalArgumentException | IllegalAccessException e) {
			throw new ServiceException("Error setting final field on" + obj + ", field: " + fieldName + " to " + val);

		}
	}
}
