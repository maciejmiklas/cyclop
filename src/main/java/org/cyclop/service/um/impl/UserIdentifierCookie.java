package org.cyclop.service.um.impl;

import org.cyclop.model.UserIdentifier;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class UserIdentifierCookie {

	private UserIdentifier id;

	@Override
	public String toString() {
		return "UserIdentifierCookie [id=" + id + "]";
	}

	public UserIdentifierCookie() {
	}

	public UserIdentifierCookie(UserIdentifier id) {
		this.id = id;
	}

	public UserIdentifier getId() {
		return id;
	}

}
