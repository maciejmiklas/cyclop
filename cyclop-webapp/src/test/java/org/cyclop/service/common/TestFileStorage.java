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
package org.cyclop.service.common;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;

import javax.inject.Inject;

import org.cyclop.common.AppConfig;
import org.cyclop.model.CqlQuery;
import org.cyclop.model.CqlQueryType;
import org.cyclop.model.QueryEntry;
import org.cyclop.model.QueryFavourites;
import org.cyclop.model.QueryHistory;
import org.cyclop.model.UserIdentifier;
import org.cyclop.model.exception.BeanValidationException;
import org.cyclop.test.AbstractTestCase;
import org.junit.Before;
import org.junit.Test;

/** @author Maciej Miklas */
public class TestFileStorage extends AbstractTestCase {

    @Inject
    private AppConfig config;

    @Inject
    private FileStorage storage;

    @Before
    public void setup() throws Exception {
	super.setup();
	File histFolder = new File(config.fileStore.folder);
	histFolder.setWritable(true);
	assertTrue("History folder not writable:" + histFolder, histFolder.canWrite());
    }

    @Test(expected = BeanValidationException.class)
    public void testRead_Validation() {
	storage.read(new UserIdentifier(), null);
    }

    @Test(expected = BeanValidationException.class)
    public void testRead_Validation_IncorrectUserIdentifier() {
	storage.read(new UserIdentifier(null), String.class);
    }

    @Test(expected = BeanValidationException.class)
    public void testStore_Validation() {
	storage.store(new UserIdentifier(), null);
    }

    @Test(expected = BeanValidationException.class)
    public void testStore_Validation_IncorrectUserIdentifier() {
	storage.store(new UserIdentifier(null), String.class);
    }

    @Test
    public void testSupported_OnWritableFolder() {
	assertTrue(storage.supported());
	assertTrue(storage.checkSupported());
    }

    @Test
    public void testSupported_OnReadOnlyFolder() {

	if (!unixOs) {
	    return;
	}
	File histFolder = new File(config.fileStore.folder);
	histFolder.setWritable(false);

	assertTrue(storage.supported());
	assertFalse(storage.checkSupported());
    }

    @Test
    public void testRead_FileNotFound() {
	assertFalse(storage.read(new UserIdentifier(UUID.randomUUID()), QueryHistory.class).isPresent());
    }

    @Test
    public void testEmptyHistory() {
	QueryHistory hist = new QueryHistory();
	assertEquals(0, hist.size());
	try (QueryHistory.HistoryIterator iterator = hist.iterator()) {
	    assertFalse(iterator.hasNext());
	}

	CqlQuery query = new CqlQuery(CqlQueryType.SELECT, "select * from MyTable");
	QueryEntry histEntry = new QueryEntry(query, 234);
	assertFalse(hist.contains(histEntry));
	try {
	    hist.iterator().next();
	    fail();
	}
	catch (NoSuchElementException e) {
	    // OK
	}
    }

    @Test
    public void testEmptyFavourites() {
	QueryFavourites hist = new QueryFavourites();
	assertEquals(0, hist.size());
	assertFalse(hist.copyAsSortedSet().iterator().hasNext());

	CqlQuery query = new CqlQuery(CqlQueryType.SELECT, "select * from MyTable");
	QueryEntry histEntry = new QueryEntry(query, 6456);
	assertFalse(hist.contains(histEntry));
	try {
	    hist.copyAsSortedSet().iterator().next();
	    fail();
	}
	catch (NoSuchElementException e) {
	    // OK
	}
    }

    @Test
    public void testCreateAndRead_SingleHistoryEntry() {
	UserIdentifier userId = new UserIdentifier(UUID.randomUUID());
	QueryHistory newHistory = new QueryHistory();

	CqlQuery query = new CqlQuery(CqlQueryType.SELECT, "select * from MyTable");
	QueryEntry histEntry = new QueryEntry(query, 6645);
	newHistory.add(histEntry);

	storage.store(userId, newHistory);

	Optional<QueryHistory> readHistory = storage.read(userId, QueryHistory.class);
	assertNotNull(readHistory);
	assertEquals(1, readHistory.get().size());
	int idx = 0;
	List<QueryEntry> histList = readHistory.get().copyAsList();
	try (QueryHistory.HistoryIterator historyIterator = readHistory.get().iterator()) {
	    assertTrue(historyIterator.hasNext());
	    QueryEntry readEntry = historyIterator.next();
	    assertEquals(histList.get(idx++), readEntry);
	    assertEquals(query, readEntry.query);
	    assertEquals(histEntry.executedOnUtc.toString(), readEntry.executedOnUtc.toString());

	    assertEquals(0, storage.getLockRetryCount());
	}
    }

    @Test
    public void testCreateAndRead_SingleFavouritesEntry() {
	UserIdentifier userId = new UserIdentifier(UUID.randomUUID());
	QueryFavourites newHistory = new QueryFavourites();

	CqlQuery query = new CqlQuery(CqlQueryType.SELECT, "select * from MyTable");
	QueryEntry favEntry = new QueryEntry(query, 234);
	newHistory.addWithSizeCheck(favEntry);

	storage.store(userId, newHistory);

	Optional<QueryFavourites> readHistory = storage.read(userId, QueryFavourites.class);
	assertNotNull(readHistory);
	assertEquals(1, readHistory.get().size());
	assertTrue(readHistory.get().copyAsSortedSet().iterator().hasNext());
	QueryEntry readEntry = readHistory.get().copyAsSortedSet().iterator().next();
	assertEquals(query, readEntry.query);
	assertEquals(favEntry.executedOnUtc.toString(), readEntry.executedOnUtc.toString());
	assertEquals(0, storage.getLockRetryCount());
    }

    @Test
    public void testLimitFavourites() {
	UserIdentifier userId = new UserIdentifier(UUID.randomUUID());
	QueryFavourites history = new QueryFavourites();
	{
	    for (int i = 0; i < 50; i++) {
		CqlQuery query = new CqlQuery(CqlQueryType.SELECT, "select * from MyTable where id=" + i);
		assertTrue(history.addWithSizeCheck(new QueryEntry(query, 234)));
	    }
	    assertEquals(50, history.size());
	}

	checkFavLimitContent(history);

	{
	    for (int i = 0; i < 10; i++) {
		CqlQuery query = new CqlQuery(CqlQueryType.SELECT, "select * from MyTableA where id=" + i);
		assertFalse(history.addWithSizeCheck(new QueryEntry(query, 456)));
	    }
	    checkFavLimitContent(history);
	}

	{
	    for (int i = 0; i < 20; i++) {
		CqlQuery query = new CqlQuery(CqlQueryType.SELECT, "select * from MyTable where id=" + i);
		assertTrue(history.addWithSizeCheck(new QueryEntry(query, 234)));
	    }
	    checkFavLimitContent(history);
	}

	{
	    storage.store(userId, history);
	    Optional<QueryFavourites> readHistory = storage.read(userId, QueryFavourites.class);
	    checkFavLimitContent(readHistory.get());
	    checkFavLimitContent(history);
	}
    }

    private void checkFavLimitContent(QueryFavourites history) {
	Set<QueryEntry> fav = history.copyAsSortedSet();
	Iterator<QueryEntry> favIt = fav.iterator();
	assertEquals(50, fav.size());
	assertEquals(50, history.size());
	for (int i = 0; i < 50; i++) {
	    assertTrue(favIt.hasNext());
	    CqlQuery query = new CqlQuery(CqlQueryType.SELECT, "select * from MyTable where id=" + i);
	    assertTrue(history.contains(new QueryEntry(query, 7754)));
	    assertTrue(history.contains(favIt.next()));
	}
    }

    private void testHistRange(QueryHistory history, int from, int to) {
	try (QueryHistory.HistoryIterator hit = history.iterator()) {
	    for (int i = from; i >= to; i--) {
		assertTrue(hit.hasNext());
		QueryEntry entry = hit.next();
		assertTrue(history.contains(entry));
		assertNotNull(entry.executedOnUtc);
		assertEquals("select * from MyTable1 where id=" + i, entry.query.part);
	    }
	}
    }

    @Test
    public void testEvictHistory() {
	UserIdentifier userId = new UserIdentifier(UUID.randomUUID());

	{
	    QueryHistory history = new QueryHistory();

	    for (int i = 0; i < 600; i++) {
		CqlQuery query = new CqlQuery(CqlQueryType.SELECT, "select * from MyTable1 where id=" + i);
		history.add(new QueryEntry(query, 4563));
	    }
	    assertEquals(500, history.size());
	    testHistRange(history, 599, 100);
	    storage.store(userId, history);
	}

	{
	    Optional<QueryHistory> history = storage.read(userId, QueryHistory.class);
	    assertNotNull(history);
	    assertEquals(500, history.get().size());
	    testHistRange(history.get(), 599, 100);

	    for (int i = 0; i < 10; i++) {
		CqlQuery query = new CqlQuery(CqlQueryType.SELECT, "select * from MyTable2 where id=" + i);
		history.get().add(new QueryEntry(query, 567));
	    }
	    storage.store(userId, history.get());
	}

	{
	    QueryHistory history = storage.read(userId, QueryHistory.class).get();
	    assertNotNull(history);
	    assertEquals(500, history.size());

	    try (QueryHistory.HistoryIterator hit = history.iterator()) {
		for (int i = 9; i >= 0; i--) {
		    assertTrue(hit.hasNext());
		    QueryEntry entry = hit.next();
		    assertTrue(history.contains(entry));
		    assertNotNull(entry.executedOnUtc);
		    assertEquals("select * from MyTable2 where id=" + i, entry.query.part);
		}
		for (int i = 599; i > 110; i--) {
		    assertTrue(hit.hasNext());
		    QueryEntry entry = hit.next();
		    assertTrue(history.contains(entry));
		    assertNotNull(entry.executedOnUtc);
		    assertEquals("select * from MyTable1 where id=" + i, entry.query.part);
		}
	    }
	}
    }

}
