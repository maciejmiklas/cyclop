package org.cyclop.common;

import org.cyclop.model.exception.ServiceException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Field;

public class SerializationUtil {
	private final static Logger LOG = LoggerFactory.getLogger(SerializationUtil.class);

	public static void setFiled(Object obj, String fieldName, Object val) {
		try {
			Class<?> clazz = obj.getClass();
			Field field = findField(clazz, fieldName);
			if (field == null) {
				throw new ServiceException("Field: " + fieldName + " not found on in class heirarchy: " + clazz);
			}
			field.setAccessible(true);
			field.set(obj, val);
		} catch (SecurityException | IllegalArgumentException | IllegalAccessException e) {
			throw new ServiceException(
					"Error setting final field: " + fieldName + " on: " + obj + " to: " + val + " - Error: " +
							e.getClass() + ", Msg:  " + e.getMessage(), e);

		}
	}

	private static Field findField(Class<?> clazz, String fieldName) {
		Field field = null;

		Class<?> fieldClass = clazz;
		do {
			try {
				field = fieldClass.getDeclaredField(fieldName);
			} catch (NoSuchFieldException e) {
				if (LOG.isDebugEnabled()) {
					LOG.debug("Field: {} not found on: {}, message:{}", fieldName, fieldClass, e.getMessage());
				}
			}
			fieldClass = fieldClass.getSuperclass();
		} while (fieldClass != null && field == null);
		return field;
	}
}
