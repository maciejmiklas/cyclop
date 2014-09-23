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
package org.cyclop.service.converter;

import java.io.IOException;
import java.io.UnsupportedEncodingException;

import javax.inject.Named;
import javax.validation.constraints.NotNull;

import org.codehaus.jackson.map.AnnotationIntrospector;
import org.codehaus.jackson.map.DeserializationConfig;
import org.codehaus.jackson.map.ObjectMapper;
import org.codehaus.jackson.map.SerializationConfig;
import org.codehaus.jackson.map.annotate.JsonSerialize;
import org.codehaus.jackson.map.introspect.JacksonAnnotationIntrospector;
import org.codehaus.jackson.xc.JaxbAnnotationIntrospector;
import org.cyclop.model.exception.ServiceException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/** @author Maciej Miklas */
@Named
public class JsonMarshaller {
	private final static Logger LOG = LoggerFactory.getLogger(JsonMarshaller.class);

	private final ThreadLocal<ObjectMapper> objectMapper;

	public JsonMarshaller() {

		objectMapper = new ThreadLocal<ObjectMapper>() {
			@Override
			protected ObjectMapper initialValue() {

				ObjectMapper mapper = new ObjectMapper();
				mapper.configure(DeserializationConfig.Feature.FAIL_ON_UNKNOWN_PROPERTIES, false);
				AnnotationIntrospector introspector = new AnnotationIntrospector.Pair(new JaxbAnnotationIntrospector(),
						new JacksonAnnotationIntrospector());

				SerializationConfig sc = mapper.getSerializationConfig().withSerializationInclusion(
						JsonSerialize.Inclusion.NON_NULL);
				mapper.setSerializationConfig(sc.withAnnotationIntrospector(introspector));
				mapper.setDeserializationConfig(mapper.getDeserializationConfig().withAnnotationIntrospector(
						introspector));
				return mapper;
			}
		};
	}

	public @NotNull <T> T unmarshal(@NotNull Class<T> clazz, @NotNull String input) {

		T unmarshalObj;
		try {
			unmarshalObj = objectMapper.get().readValue(input, clazz);
		} catch (IOException e) {
			throw new ServiceException("Got IOException during json unmarshalling: " + e.getMessage() + " - input:'"
					+ input + "'", e);
		}
		LOG.trace("Unmarshaled JSON from {} to {}", input, unmarshalObj);
		return unmarshalObj;
	}

	public @NotNull String marshal(@NotNull Object obj) {
		byte[] marshalBytes;
		try {
			marshalBytes = objectMapper.get().writeValueAsBytes(obj);
		} catch (IOException e) {
			throw new ServiceException("Gout IOException during json marshalling: " + e.getMessage() + " - input:'"
					+ obj + "'", e);
		}

		try {
			String marshal = new String(marshalBytes, "UTF-8");
			LOG.trace("Marshalled JSON from {} to {}", obj, marshal);
			return marshal;
		} catch (UnsupportedEncodingException e) {
			throw new ServiceException("UnsupportedEncodingException marshalling Json stream: " + e.getMessage()
					+ " - input:'" + obj + "'", e);
		}

	}

}
