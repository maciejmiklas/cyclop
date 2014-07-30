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
package org.cyclop.validation;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.locks.Lock;

import javax.validation.ConstraintViolation;
import javax.validation.Path;
import javax.validation.Validation;
import javax.validation.Validator;
import javax.validation.metadata.ConstraintDescriptor;

import org.cyclop.model.Synchronizable;
import org.cyclop.model.exception.BeanValidationException;

import com.google.common.collect.ImmutableSet;

/** @author Maciej Miklas */
public final class BeanValidator {
	private final static String NULL_MARKER = "NULL-" + UUID.randomUUID();

	private final static Validator VALIDATOR = Validation.buildDefaultValidatorFactory().getValidator();

	private Map<String, Object> objMap = new HashMap<>();

	public static BeanValidator create() {
		return new BeanValidator();
	}

	public static BeanValidator create(Object obj) {
		return new BeanValidator().add(obj);
	}

	public BeanValidator add(Object obj) {
		if (obj == null) {
			throw new IllegalArgumentException("null validation object");
		}
		return add(obj.getClass().getSimpleName(), obj);
	}

	public BeanValidator add(String name, Object obj) {
		if (name == null) {
			throw new IllegalArgumentException("null name for validation object");
		}

		String key = objMap.containsKey(name) ? name + " - " + UUID.randomUUID() : name;
		objMap.put(key, obj == null ? NULL_MARKER : obj);
		return this;
	}

	public void validate() {
		Map<String, Set<ConstraintViolation<Object>>> violations = new HashMap<>();

		for (Map.Entry<String, Object> obj : objMap.entrySet()) {
			Object value = obj.getValue();
			if (value.toString().equals(NULL_MARKER)) {
				violations.put(obj.getKey(), createViolationForNullRoot());
				continue;
			}
			Set<ConstraintViolation<Object>> violation = validateObject(value);
			if (!violation.isEmpty()) {
				violations.put(obj.getKey(), violation);
			}
		}

		if (!violations.isEmpty()) {
			throw new BeanValidationException(violations);
		}
	}

	private Set<ConstraintViolation<Object>> validateObject(Object obj) {
		if (obj instanceof Optional) {
			Optional<?> opt = (Optional<?>) obj;
			if (opt.isPresent()) {
				obj = opt.get();
			}
		}

		Set<ConstraintViolation<Object>> violation = null;
		if (obj instanceof Synchronizable) {
			Lock lock = ((Synchronizable) obj).getLock();
			lock.lock();
			try {
				violation = VALIDATOR.validate(obj);
			} finally {
				lock.unlock();
			}
		} else {
			violation = VALIDATOR.validate(obj);
		}
		return violation;
	}

	private Set<ConstraintViolation<Object>> createViolationForNullRoot() {
		ConstraintViolation<Object> viol = new NullRootViolation();
		Set<ConstraintViolation<Object>> violSet = ImmutableSet.of(viol);
		return violSet;
	}

	public final class NullRootViolation implements ConstraintViolation<Object> {
		private String message = "NULL_ROOT_OBJECT";

		@Override
		public ConstraintDescriptor<?> getConstraintDescriptor() {
			return null;
		}

		@Override
		public Object getInvalidValue() {
			return null;
		}

		@Override
		public Object getLeafBean() {
			return null;
		}

		@Override
		public String getMessage() {
			return message;
		}

		@Override
		public String getMessageTemplate() {
			return message;
		}

		@Override
		public Path getPropertyPath() {
			return null;
		}

		@Override
		public Object getRootBean() {
			return null;
		}

		@Override
		public Class<Object> getRootBeanClass() {
			return null;
		}

		@Override
		public String toString() {
			return message;
		}

	}
}
