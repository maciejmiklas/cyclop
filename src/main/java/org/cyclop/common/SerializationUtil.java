/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.cyclop.common;

import org.cyclop.model.exception.ServiceException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Field;

public class SerializationUtil {
	private final static Logger LOG = LoggerFactory.getLogger(SerializationUtil.class);

	public static void setFiled(Object obj, String fieldName, Object val) {
		LOG.debug("Set filed {} to {} on {}", fieldName, val, obj);

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

		LOG.debug("Found filed {} on {}", fieldName, clazz);
		return field;
	}
}
