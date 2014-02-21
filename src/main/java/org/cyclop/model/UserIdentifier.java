package org.cyclop.model;

import com.google.common.base.Objects;
import net.jcip.annotations.Immutable;

import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.adapters.XmlAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import java.io.Serializable;
import java.util.UUID;

/** @author Maciej Miklas */
@Immutable
@XmlJavaTypeAdapter(value = UserIdentifier.Adapter.class)
public final class UserIdentifier implements Serializable {

	public final UUID id;

	public UserIdentifier() {
		id = UUID.randomUUID();
	}

	public UserIdentifier(UUID id) {
		this.id = id;
		if (id == null) {
			throw new IllegalArgumentException();
		}
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
