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

import java.io.File;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.channels.FileLockInterruptionException;
import java.nio.channels.OverlappingFileLockException;
import java.nio.charset.Charset;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.CharsetEncoder;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicInteger;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;
import javax.validation.Valid;
import javax.validation.constraints.NotNull;

import net.jcip.annotations.NotThreadSafe;

import org.apache.commons.lang3.StringUtils;
import org.cyclop.common.AppConfig;
import org.cyclop.model.UserIdentifier;
import org.cyclop.model.exception.ServiceException;
import org.cyclop.service.converter.JsonMarshaller;
import org.cyclop.validation.EnableValidation;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/** @author Maciej Miklas */
@Named
@NotThreadSafe
@EnableValidation
public class FileStorage {
	private final static Logger LOG = LoggerFactory.getLogger(FileStorage.class);

	private ThreadLocal<CharsetEncoder> encoder;

	private ThreadLocal<CharsetDecoder> decoder;

	@Inject
	private AppConfig config;

	@Inject
	private JsonMarshaller jsonMarshaller;

	private boolean supported;

	private final AtomicInteger lockRetryCount = new AtomicInteger(0);

	@PostConstruct
	protected void init() {
		supported = checkSupported();

		encoder = new ThreadLocal<CharsetEncoder>() {
			@Override
			protected CharsetEncoder initialValue() {
				Charset charset = Charset.forName("UTF-8");
				CharsetEncoder decoder = charset.newEncoder();
				return decoder;
			}
		};

		decoder = new ThreadLocal<CharsetDecoder>() {
			@Override
			protected CharsetDecoder initialValue() {
				Charset charset = Charset.forName("UTF-8");
				CharsetDecoder decoder = charset.newDecoder();
				return decoder;
			}
		};
	}

	public boolean supported() {
		return supported;
	}

	protected boolean checkSupported() {
		if (!config.history.enabled) {
			LOG.info("Query history is disabled");
			return false;
		}

		File histFolder = new File(config.fileStore.folder);
		if (!histFolder.exists()) {
			LOG.warn("Query history is enabled, but configured folder does not exists:{}", histFolder);
			return false;
		}

		if (!histFolder.canWrite()) {
			LOG.warn("Query history is enabled, but configured folder is read-only:{}", histFolder);
			return false;
		}

		return true;
	}

	public void store(@NotNull UserIdentifier userId, @NotNull Object entity) throws ServiceException {
		LOG.debug("Storing file for {}", userId);
		Path histPath = getPath(userId, entity.getClass());
		try (FileChannel channel = openForWrite(histPath)) {
			String jsonText = jsonMarshaller.marshal(entity);
			ByteBuffer buf = encoder.get().encode(CharBuffer.wrap(jsonText));
			int written = channel.write(buf);
			channel.truncate(written);
		} catch (IOException | SecurityException | IllegalStateException e) {
			throw new ServiceException("Error storing query history in:" + histPath + " - " + e.getClass() + " - "
					+ e.getMessage(), e);
		}
		LOG.trace("File has been sotred {}", entity);
	}

	public @Valid <T> Optional<T> read(@NotNull UserIdentifier userId, @NotNull Class<T> clazz) throws ServiceException {
		Path filePath = getPath(userId, clazz);
		LOG.debug("Reading file {} for {}", filePath, userId);
		try (FileChannel channel = openForRead(filePath)) {
			if (channel == null) {
				LOG.debug("File not found: {}", filePath);
				return Optional.empty();
			}
			int fileSize = (int) channel.size();
			if (fileSize > config.fileStore.maxFileSize) {
				LOG.info("File: {} too large: {} - skipping it", filePath, fileSize);
				return Optional.empty();
			}
			ByteBuffer buf = ByteBuffer.allocate(fileSize);
			channel.read(buf);
			buf.flip();
			String decoded = decoder.get().decode(buf).toString();
			decoded = StringUtils.trimToNull(decoded);
			if(decoded == null) {
				return Optional.empty();
			}
			T content = jsonMarshaller.unmarshal(clazz, decoded);

			LOG.debug("File read");
			return Optional.ofNullable(content);
		} catch (IOException | SecurityException | IllegalStateException e) {
			throw new ServiceException("Error reading filr from:" + filePath + " - " + e.getMessage(), e);
		}

	}

	private FileChannel openForWrite(Path histPath) throws IOException {
		FileChannel byteChannel = FileChannel.open(histPath, StandardOpenOption.CREATE,
				StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.WRITE);
		byteChannel.force(true);
		FileChannel lockChannel = lock(histPath, byteChannel);
		return lockChannel;
	}

	private FileChannel openForRead(Path histPath) throws IOException {
		File file = histPath.toFile();
		if (!file.exists() || !file.canRead()) {
			LOG.debug("History file not found: " + histPath);
			return null;
		}
		FileChannel byteChannel = FileChannel.open(histPath, StandardOpenOption.READ, StandardOpenOption.WRITE);
		FileChannel lockChannel = lock(histPath, byteChannel);
		return lockChannel;
	}

	private FileChannel lock(Path histPath, FileChannel channel) throws IOException {
		LOG.debug("Trying to log file: {}", histPath);
		long start = System.currentTimeMillis();
		String lastExMessage = null;
		FileChannel lockChannel = null;
		while (lockChannel == null && System.currentTimeMillis() - start < config.fileStore.lockWaitTimeoutMillis) {
			try {
				FileLock lock = channel.lock();
				lockChannel = lock.channel();
			} catch (FileLockInterruptionException | OverlappingFileLockException e) {
				lockRetryCount.incrementAndGet();
				lastExMessage = e.getMessage();
				LOG.debug("File lock on '{}' cannot be obtained (retrying operation): {}", histPath, lastExMessage);
				try {
					Thread.sleep(100);
				} catch (InterruptedException e1) {
					Thread.interrupted();
				}
			}
		}
		if (lockChannel == null) {
			throw new ServiceException("File lock on '" + histPath + "' cannot be obtained: " + lastExMessage);
		}

		return lockChannel;
	}

	private Path getPath(UserIdentifier userId, Class<?> entity) {
		String fileName = entity.getSimpleName() + "-" + userId.id + ".json";
		Path histPath = Paths.get(config.fileStore.folder, fileName);
		return histPath;
	}

	public int getLockRetryCount() {
		return lockRetryCount.get();
	}

}
