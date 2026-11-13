//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#ifdef _MSC_VER
#pragma warning(disable : 4251)
#endif
//=============================================================================
#include <cstdint>
#include <cstdio>
#include <string>
#include <vector>
#include "Types.hpp"
#include "nlsStream_manager_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
// Byte stream behind a file identifier that is not a C file (ZIP entry, Zstandard data).
// Readers only produce bytes forward: this class buffers them and emulates positioning.
class NLSSTREAM_MANAGER_IMPEXP FileBackend
{
public:
    FileBackend(const FileBackend&) = delete;
    FileBackend&
    operator=(const FileBackend&) = delete;
    virtual ~FileBackend();

    bool
    isWritable() const;
    // Copies up to size bytes; a short count sets the end-of-file flag.
    size_t
    read(void* data, size_t size);
    // Next line with its LF when present; false when no byte is left.
    bool
    readLine(std::string& line);
    bool
    eof() const;
    size_t
    write(const void* data, size_t size);
    int64
    tell() const;
    // Moves to an absolute position (readers only); false leaves the position unchanged.
    // The first backward jump decodes again once and caches bytes, later ones are fast.
    bool
    seek(int64 position);
    // Size in bytes; a reader of unknown size is scanned once to find it.
    int64
    size();
    // Cache file holding the bytes from position to the end, positioned there (for fscanf).
    FILE*
    scanCopy(int64 position, int64& copyStart);
    // Finishes a writer and releases the stream; raises its errors once.
    void
    close();

protected:
    explicit FileBackend(bool writable);
    // Next decompressed bytes, 0 at the end of the stream.
    virtual size_t
    produce(uint8_t* buffer, size_t capacity) = 0;
    // Starts the stream again from its first byte.
    virtual void
    restart() = 0;
    virtual void
    consume(const uint8_t* data, size_t size) = 0;
    // Called once by close() for writers.
    virtual void
    finish() = 0;
    // Size known without reading, -1 otherwise.
    virtual int64
    knownSize() const;
    // Random access: restarts produce() at a block start <= position and returns it, -1 if
    // unsupported.
    virtual int64
    restartNear(int64 position);
    // For derived destructors: finishes a writer that was not closed, ignoring errors.
    void
    closeQuietly() noexcept;

private:
    bool writable;
    bool closed = false;
    bool endReached = false;
    bool eofFlag = false;
    std::vector<uint8_t> buffer;
    size_t bufferLength = 0;
    size_t bufferOffset = 0;
    int64 bufferStart = 0;
    int64 totalSize = -1;
    int64 written = 0;
    // Position of the next byte that produce() returns.
    int64 streamPosition = 0;
    // Decoded bytes [cacheBase, cacheEnd) kept on disk; the stream always writes at cacheEnd.
    FILE* cacheFile = nullptr;
    std::wstring cachePath;
    int64 cacheBase = 0;
    int64 cacheEnd = 0;

    bool
    fillAt(int64 start);
    size_t
    produceNext();
    void
    rewindStream();
    // Restarts near position when random access is possible; otherwise rewinds if asked.
    bool
    repositionStream(int64 position, bool rewindIfUnsupported);
    bool
    prepareCache(int64 position);
    void
    drainToEnd(int64 resumeAt);
    void
    releaseCache();
};
//=============================================================================
} // namespace Nelson
//=============================================================================
