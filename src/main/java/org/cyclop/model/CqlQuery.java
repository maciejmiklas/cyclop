package org.cyclop.model;

import com.google.common.base.Objects;
import net.jcip.annotations.Immutable;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.adapters.XmlAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/** @author Maciej Miklas */
@Immutable
@XmlJavaTypeAdapter(CqlQuery.Adapter.class)
public class CqlQuery extends CqlPart {

	public final CqlQueryName type;

	public CqlQuery(CqlQueryName type, String cql) {
		super(cql);
		if (type == null) {
			throw new IllegalArgumentException("Null type");
		}
		this.type = type;
	}

	@Override
	public String toString() {
		return Objects.toStringHelper(this).add("part", part).add("type", type).toString();
	}

	@XmlRootElement
	@XmlAccessorType(XmlAccessType.FIELD)
	public final static class CqlQueryJaxb {
		public String cql;

		public CqlQueryName type;
	}

	@XmlTransient
	public final static class Adapter extends XmlAdapter<CqlQueryJaxb, CqlQuery> {

		@Override
		public CqlQuery unmarshal(CqlQueryJaxb jaxb) throws Exception {
			if (jaxb == null) {
				return null;
			}

			CqlQuery entry = new CqlQuery(jaxb.type, jaxb.cql);
			return entry;
		}

		@Override
		public CqlQueryJaxb marshal(CqlQuery histObj) throws Exception {
			if (histObj == null) {
				return null;
			}
			CqlQueryJaxb jaxb = new CqlQueryJaxb();
			jaxb.cql = histObj.part;
			jaxb.type = histObj.type;
			return jaxb;
		}
	}
}
