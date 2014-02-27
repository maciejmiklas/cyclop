package org.cyclop.service.converter;

import org.apache.commons.lang.StringUtils;
import org.codehaus.jackson.map.AnnotationIntrospector;
import org.codehaus.jackson.map.DeserializationConfig;
import org.codehaus.jackson.map.ObjectMapper;
import org.codehaus.jackson.map.SerializationConfig;
import org.codehaus.jackson.map.annotate.JsonSerialize;
import org.codehaus.jackson.map.introspect.JacksonAnnotationIntrospector;
import org.codehaus.jackson.xc.JaxbAnnotationIntrospector;
import org.cyclop.model.exception.ServiceException;

import javax.inject.Named;
import javax.validation.Valid;
import java.io.IOException;
import java.io.UnsupportedEncodingException;

/** @author Maciej Miklas */
@Named
public class JsonMarshaller {

	private final ThreadLocal<ObjectMapper> objectMapper;

	public JsonMarshaller() {

		objectMapper = new ThreadLocal<ObjectMapper>() {
			@Override
			protected ObjectMapper initialValue() {

				ObjectMapper mapper = new ObjectMapper();
				mapper.configure(DeserializationConfig.Feature.FAIL_ON_UNKNOWN_PROPERTIES, false);
				AnnotationIntrospector introspectorPair = new AnnotationIntrospector.Pair(
						new JaxbAnnotationIntrospector(), new JacksonAnnotationIntrospector());

				SerializationConfig sc = mapper.getSerializationConfig()
						.withSerializationInclusion(JsonSerialize.Inclusion.NON_NULL);
				mapper.setSerializationConfig(sc.withAnnotationIntrospector(introspectorPair));
				mapper.setDeserializationConfig(
						mapper.getDeserializationConfig().withAnnotationIntrospector(introspectorPair));
				return mapper;
			}
		};
	}

	public <T> T unmarshal(Class<T> clazz, String input) {
		input = StringUtils.trimToNull(input);
		if (input == null) {
			return null;
		}

		T unmarshalObj = null;
		try {
			unmarshalObj = objectMapper.get().readValue(input, clazz);
		} catch (IOException e) {
			throw new ServiceException(
					"Got IOException during json unmarshalling: " + e.getMessage() + " - input:'" + input + "'", e);
		}
		return unmarshalObj;
	}

	public String marshal(@Valid Object obj) {
		if (obj == null) {
			return null;
		}
		byte[] marshalBytes;
		try {
			marshalBytes = objectMapper.get().writeValueAsBytes(obj);
		} catch (IOException e) {
			throw new ServiceException(
					"Gout IOException during json marshalling: " + e.getMessage() + " - input:'" + obj + "'", e);
		}

		try {
			return new String(marshalBytes, "UTF-8");
		} catch (UnsupportedEncodingException e) {
			throw new ServiceException(
					"UnsupportedEncodingException marshalling Json stream: " + e.getMessage() + " - input:'" + obj +
							"'", e);
		}

	}

}
