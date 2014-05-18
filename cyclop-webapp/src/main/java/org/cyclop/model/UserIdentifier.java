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
package org.cyclop.model;

import com.google.common.base.Objects;
import net.jcip.annotations.Immutable;

import javax.validation.constraints.NotNull;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.adapters.XmlAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import java.io.Serializable;
import java.util.UUID;

/** @author Maciej Miklas */
@Immutable
@XmlJavaTypeAdapter(value = UserIdentifier.Adapter.class)
public final class UserIdentifier implements Serializable {

	@NotNull
	public final UUID id;

	public UserIdentifier() {
		id = UUID.randomUUID();
	}

	public UserIdentifier(UUID id) {
		this.id = id;
	}

	@Override
	public String toString() {
		return Objects.toStringHelper(this).add("id", id).toString();
	}

	@Override
	public int hashCode() {
		return java.util.Objects.hash(id);
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == null || getClass() != obj.getClass()) {
			return false;
		}

		final UserIdentifier other = (UserIdentifier) obj;
		return java.util.Objects.equals(id, other.id);
	}

	@XmlTransient
	public static class Adapter extends XmlAdapter<String, UserIdentifier> {

		@Override
		public UserIdentifier unmarshal(String str) {
			if (str == null) {
				return null;
			}
			UUID uuid = UUID.fromString(str);
			return new UserIdentifier(uuid);
		}

		@Override
		public String marshal(UserIdentifier id) {
			if (id == null) {
				return null;
			}
			return id.id.toString();
		}
	}
}
