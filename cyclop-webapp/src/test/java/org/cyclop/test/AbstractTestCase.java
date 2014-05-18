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

import com.datastax.driver.core.Session;
import org.apache.cassandra.io.util.FileUtils;
import org.cyclop.service.cassandra.CassandraSession;
import org.junit.After;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.runner.RunWith;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.web.WebAppConfiguration;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;

import javax.inject.Inject;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;

/** @author Maciej Miklas */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = {TestProps.TEST_CONTEXT})
@WebAppConfiguration
@EnableWebMvc
public abstract class AbstractTestCase {

	private static final EmbeddedCassandra CASSANDRA = new EmbeddedCassandra();

	private static boolean INIT_EXECUTED = false;

	protected final boolean unixOs = !System.getProperty("os.name", "linux").toLowerCase().contains("windows");

	@Inject
	protected CassandraSession cassandraSession;

	private static void rmdir(Path dir) throws IOException {
		File dirFile = dir.toFile();
		if (dirFile.exists()) {
			FileUtils.deleteRecursive(dirFile);
		}
	}

	private static void setupCassandra() throws Exception {
		CASSANDRA.start();
	}

	private static void setupHistory() throws Exception {
		Path tempPath = FileSystems.getDefault().getPath("target", "cyclop-history-test");
		rmdir(tempPath);
		Files.createDirectory(tempPath);
		System.getProperties().setProperty("fileStore.folder", tempPath.toString());
	}

	@BeforeClass
	public static void staticInit() throws Exception {
		if (INIT_EXECUTED) {
			return;
		}
		INIT_EXECUTED = true;
		setupHistory();
		setupCassandra();
	}

	public <T> T deserialize(byte[] serialized, Class<T> clazz) throws IOException, ClassNotFoundException {
		ObjectInputStream in = new ObjectInputStream(new ByteArrayInputStream(serialized));

		@SuppressWarnings("unchecked")
		T des = (T) in.readObject();
		in.close();
		assertNotNull(des);
		return des;
	}

	public Session getCassandraSession() {
		assertNotNull("Cassandra session is null", CASSANDRA.getSession());
		return CASSANDRA.getSession();
	}

	public byte[] serialize(Object obj) throws IOException {
		ByteArrayOutputStream bout = new ByteArrayOutputStream();
		ObjectOutputStream out = new ObjectOutputStream(bout);
		out.writeObject(obj);
		out.close();
		byte[] serialized = bout.toByteArray();
		assertNotNull(serialized);
		return serialized;
	}

	@Before
	public void setup() throws Exception {
		cassandraSession.authenticate("test", "test1234");
	}

	@After
	public void cleanUp() {
		cassandraSession.close();
		assertFalse(cassandraSession.isOpen());
	}
}
