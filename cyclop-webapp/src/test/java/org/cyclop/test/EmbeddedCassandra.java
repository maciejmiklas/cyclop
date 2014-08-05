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
package org.cyclop.test;

import static junit.framework.Assert.assertNotNull;

import java.net.URL;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.UUID;

import org.apache.commons.lang.StringUtils;
import org.cassandraunit.utils.EmbeddedCassandraServerHelper;

import com.datastax.driver.core.Cluster;
import com.datastax.driver.core.Session;

/** @author Maciej Miklas */
public class EmbeddedCassandra {

    private Cluster cluster;

    private Session session;

    private boolean running = false;

    public void start() throws Exception {
	if (running) {
	    return;
	}

	EmbeddedCassandraServerHelper.startEmbeddedCassandra("/cassandra.yaml");

	cluster = Cluster.builder().addContactPoints("localhost").withPort(9042).build();
	assertNotNull(cluster);

	session = cluster.connect();
	assertNotNull(session);

	executeScript(session, "/cql/createDemoTables.cql");
	executeScript(session, "/cql/createDemoData.cql");
	createTestData(session);
	running = true;
    }

    private void createTestData(Session session) {
	session.execute("USE CqlDemo");

	// all tables have this common value: deesc=TEST_SET_1
	{
	    for (int i = 0; i < 50; i++) {
		StringBuilder cql = new StringBuilder("INSERT INTO CompoundTest (id,id2,id3,deesc) VALUES (");
		cql.append(UUID.randomUUID()).append(",");
		cql.append(i);
		cql.append(",'abc','TEST_SET_1')");
		session.execute(cql.toString());
	    }
	}

	// all tables have this common value: pages = 2212
	{
	    for (int i = 0; i < 100; i++) {
		StringBuilder cql = new StringBuilder(
			"INSERT INTO MyBooks (id,title,genre,publishDate,description,authors,pages,price,idx) VALUES (");
		cql.append(UUID.randomUUID()).append(",");
		cql.append("'Midnight Rain-").append(i).append("',");
		if (i == 0) {
		    cql.append("'Fantasy',");
		}
		else {
		    cql.append("null,");
		}
		cql.append("'2000-10-01',");

		cql.append("'Description.....-").append(i).append("',");
		cql.append("{'Ralls, Kim'},2212, {'D':2.85,'E':3.11,'F':4.22},");
		cql.append(i).append(")");
		session.execute(cql.toString());
	    }
	}
    }

    private void executeScript(Session session, String scriptName) throws Exception {
	URL scriptURL = getClass().getResource(scriptName);
	assertNotNull(scriptName, scriptURL);
	Path scriptPath = Paths.get(scriptURL.toURI());
	List<String> lines = Files.readAllLines(scriptPath, Charset.forName("UTF-8"));
	for (String line : lines) {
	    line = line.replaceAll("\\p{Cc}", "");
	    line = StringUtils.trimToNull(line);
	    if (line == null) {
		continue;
	    }
	    try {
		session.execute(line);
	    }
	    catch (Exception e) {
		throw new Exception("Error executing:" + line + " - got: " + e.getMessage(), e);
	    }
	}
    }

    public Cluster getCluster() {
	return cluster;
    }

    public void stop() throws Exception {
	cluster.close();
    }

    public Session getSession() {
	return session;
    }
}
