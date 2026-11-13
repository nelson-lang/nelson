//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <cstring>
#ifdef _MSC_VER
#include <share.h>
#endif
#include "FileBackend.hpp"
#include "FileSeek.hpp"
#include "FileSystemWrapper.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static constexpr size_t backendBlockSize = 64 * 1024;
//=============================================================================
static FILE*
createTemporaryFile(std::wstring& path);
//=============================================================================
FileBackend::FileBackend(bool writable) : writable(writable)
{
    if (!writable) {
        buffer.resize(backendBlockSize);
    }
}
//=============================================================================
FileBackend::~FileBackend() { releaseCache(); }
//=============================================================================
bool
FileBackend::isWritable() const
{
    return writable;
}
//=============================================================================
size_t
FileBackend::read(void* data, size_t size)
{
    if (writable || closed) {
        return 0;
    }
    auto* out = static_cast<uint8_t*>(data);
    size_t copied = 0;
    while (copied < size) {
        if (bufferOffset == bufferLength
            && !fillAt(bufferStart + static_cast<int64>(bufferLength))) {
            eofFlag = true;
            break;
        }
        const size_t count = std::min(size - copied, bufferLength - bufferOffset);
        std::memcpy(out + copied, buffer.data() + bufferOffset, count);
        bufferOffset += count;
        copied += count;
    }
    return copied;
}
//=============================================================================
bool
FileBackend::readLine(std::string& line)
{
    line.clear();
    if (writable || closed) {
        return false;
    }
    for (;;) {
        if (bufferOffset == bufferLength
            && !fillAt(bufferStart + static_cast<int64>(bufferLength))) {
            eofFlag = true;
            return !line.empty();
        }
        const uint8_t* start = buffer.data() + bufferOffset;
        const size_t available = bufferLength - bufferOffset;
        const auto* lf = static_cast<const uint8_t*>(std::memchr(start, '\n', available));
        const size_t count = lf != nullptr ? static_cast<size_t>(lf - start) + 1 : available;
        line.append(reinterpret_cast<const char*>(start), count);
        bufferOffset += count;
        if (lf != nullptr) {
            return true;
        }
    }
}
//=============================================================================
bool
FileBackend::eof() const
{
    return eofFlag;
}
//=============================================================================
size_t
FileBackend::write(const void* data, size_t size)
{
    if (!writable || closed) {
        return 0;
    }
    consume(static_cast<const uint8_t*>(data), size);
    written += static_cast<int64>(size);
    return size;
}
//=============================================================================
int64
FileBackend::tell() const
{
    return writable ? written : bufferStart + static_cast<int64>(bufferOffset);
}
//=============================================================================
bool
FileBackend::seek(int64 position)
{
    if (writable || closed || position < 0) {
        return writable && position == written;
    }
    const int64 known = knownSize() >= 0 ? knownSize() : totalSize;
    if (known >= 0 && position > known) {
        return false;
    }
    if (position >= bufferStart && position <= bufferStart + static_cast<int64>(bufferLength)) {
        bufferOffset = static_cast<size_t>(position - bufferStart);
        eofFlag = false;
        return true;
    }
    const int64 previous = tell();
    // A random access stream jumps directly, the others cache decoded bytes.
    if (position < bufferStart && cacheFile == nullptr && !repositionStream(position, false)) {
        prepareCache(position);
    }
    if (!fillAt(position) && position != totalSize) {
        seek(previous);
        return false;
    }
    eofFlag = false;
    return true;
}
//=============================================================================
int64
FileBackend::size()
{
    if (writable) {
        return written;
    }
    if (knownSize() >= 0) {
        return knownSize();
    }
    if (totalSize < 0 && !closed) {
        const int64 previous = tell();
        prepareCache(previous);
        drainToEnd(previous);
    }
    return totalSize;
}
//=============================================================================
FILE*
FileBackend::scanCopy(int64 position, int64& copyStart)
{
    if (writable || closed || !prepareCache(position)) {
        return nullptr;
    }
    drainToEnd(position);
    copyStart = cacheBase;
    NLSFSEEK(cacheFile, position - cacheBase, SEEK_SET);
    clearerr(cacheFile);
    return cacheFile;
}
//=============================================================================
void
FileBackend::close()
{
    if (closed) {
        return;
    }
    closed = true;
    releaseCache();
    if (writable) {
        finish();
    }
}
//=============================================================================
int64
FileBackend::knownSize() const
{
    return -1;
}
//=============================================================================
int64
FileBackend::restartNear(int64)
{
    return -1;
}
//=============================================================================
void
FileBackend::closeQuietly() noexcept
{
    try {
        close();
    } catch (...) {
    }
}
//=============================================================================
bool
FileBackend::fillAt(int64 start)
{
    if (cacheFile != nullptr && start >= cacheBase && start < cacheEnd) {
        NLSFSEEK(cacheFile, start - cacheBase, SEEK_SET);
        const size_t wanted
            = static_cast<size_t>(std::min(static_cast<int64>(buffer.size()), cacheEnd - start));
        bufferStart = start;
        bufferOffset = 0;
        bufferLength = fread(buffer.data(), 1, wanted, cacheFile);
        if (bufferLength > 0) {
            return true;
        }
        releaseCache();
    }
    if (start < streamPosition) {
        repositionStream(start, true);
    } else if (start - streamPosition > static_cast<int64>(4 * buffer.size())) {
        // Far forward: restarting near the target avoids decoding the bytes before it.
        repositionStream(start, false);
    }
    for (;;) {
        if (produceNext() == 0) {
            bufferStart = start;
            bufferLength = 0;
            bufferOffset = 0;
            return false;
        }
        if (start < streamPosition) {
            bufferOffset = static_cast<size_t>(start - bufferStart);
            return true;
        }
    }
}
//=============================================================================
size_t
FileBackend::produceNext()
{
    if (endReached) {
        return 0;
    }
    const size_t count = produce(buffer.data(), buffer.size());
    if (count == 0) {
        endReached = true;
        totalSize = streamPosition;
        return 0;
    }
    if (cacheFile != nullptr) {
        NLSFSEEK(cacheFile, streamPosition - cacheBase, SEEK_SET);
        if (fwrite(buffer.data(), 1, count, cacheFile) == count) {
            cacheEnd = streamPosition + static_cast<int64>(count);
        } else {
            releaseCache();
        }
    }
    bufferStart = streamPosition;
    bufferLength = count;
    bufferOffset = 0;
    streamPosition += static_cast<int64>(count);
    return count;
}
//=============================================================================
void
FileBackend::rewindStream()
{
    restart();
    streamPosition = 0;
    bufferStart = 0;
    bufferLength = 0;
    bufferOffset = 0;
    endReached = false;
    cacheBase = 0;
    cacheEnd = 0;
}
//=============================================================================
bool
FileBackend::repositionStream(int64 position, bool rewindIfUnsupported)
{
    const int64 at = restartNear(position);
    if (at < 0) {
        if (rewindIfUnsupported) {
            rewindStream();
        }
        return false;
    }
    streamPosition = at;
    bufferStart = at;
    bufferLength = 0;
    bufferOffset = 0;
    endReached = false;
    cacheBase = at;
    cacheEnd = at;
    return true;
}
//=============================================================================
bool
FileBackend::prepareCache(int64 position)
{
    if (cacheFile != nullptr) {
        if (position < cacheBase) {
            repositionStream(position, true);
        }
        return true;
    }
    cacheFile = createTemporaryFile(cachePath);
    if (cacheFile == nullptr) {
        return false;
    }
    if (position >= streamPosition) {
        cacheBase = streamPosition;
        cacheEnd = streamPosition;
    } else if (position >= bufferStart
        && bufferStart + static_cast<int64>(bufferLength) == streamPosition) {
        // The buffer is the last decoded block: it starts the cache.
        cacheBase = bufferStart;
        cacheEnd = bufferStart;
        if (fwrite(buffer.data(), 1, bufferLength, cacheFile) != bufferLength) {
            releaseCache();
            return false;
        }
        cacheEnd = streamPosition;
    } else {
        repositionStream(position, true);
    }
    return true;
}
//=============================================================================
void
FileBackend::drainToEnd(int64 resumeAt)
{
    while (produceNext() > 0) { }
    bufferStart = resumeAt;
    bufferLength = 0;
    bufferOffset = 0;
}
//=============================================================================
void
FileBackend::releaseCache()
{
    if (cacheFile != nullptr) {
        fclose(cacheFile);
        cacheFile = nullptr;
        FileSystemWrapper::Path::remove(cachePath);
        cachePath.clear();
    }
    cacheBase = 0;
    cacheEnd = 0;
}
//=============================================================================
// Unique file in the temporary folder, opened like the other Nelson files.
static FILE*
createTemporaryFile(std::wstring& path)
{
    path = FileSystemWrapper::Path::unique_path().wstring();
#ifdef _MSC_VER
    return _wfsopen(path.c_str(), L"w+b", _SH_DENYNO);
#else
    return fopen(wstring_to_utf8(path).c_str(), "w+b");
#endif
}
//=============================================================================
} // namespace Nelson
//=============================================================================
