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
#include <cstdint>
#include <fstream>
#include <memory>
#include <string>
#include <vector>
#include "FileBackend.hpp"
#include "TarArchive.hpp"
#include "ZipArchive.hpp"
#include "ZstdCodec.hpp"
#include "streamBuiltinHelpers.hpp"
//=============================================================================
// File identifier backends of nelson.io.zip.fopen, nelson.io.zstd.fopen and nelson.io.gzip.fopen.
namespace Nelson::FileArchiverGateway {
//=============================================================================
// Reads one ZIP entry through its own archive handle (several readers can coexist).
class ZipEntryReadBackend : public FileBackend
{
public:
    ZipEntryReadBackend(ZipArchive archive, size_t index, uint64_t maxSize);
    ~ZipEntryReadBackend() override;

protected:
    size_t
    produce(uint8_t* buffer, size_t capacity) override;
    void
    restart() override;
    void
    consume(const uint8_t* data, size_t size) override;
    void
    finish() override;
    int64
    knownSize() const override;

private:
    ZipArchive archive;
    size_t index;
    uint64_t maxSize;
};
//=============================================================================
// Writes the entry started in the archive session; finish ends it.
class ZipEntryWriteBackend : public FileBackend
{
public:
    explicit ZipEntryWriteBackend(uint64_t sessionId);
    ~ZipEntryWriteBackend() override;

protected:
    size_t
    produce(uint8_t* buffer, size_t capacity) override;
    void
    restart() override;
    void
    consume(const uint8_t* data, size_t size) override;
    void
    finish() override;

private:
    uint64_t sessionId;
};
//=============================================================================
// Reads one tar entry through its own archive handle.
class TarEntryReadBackend : public FileBackend
{
public:
    TarEntryReadBackend(TarArchive archive, size_t index, uint64_t maxSize);
    ~TarEntryReadBackend() override;

protected:
    size_t
    produce(uint8_t* buffer, size_t capacity) override;
    void
    restart() override;
    void
    consume(const uint8_t* data, size_t size) override;
    void
    finish() override;
    int64
    knownSize() const override;

private:
    TarArchive archive;
    size_t index;
    uint64_t maxSize;
};
//=============================================================================
// Writes a tar entry: the data is kept in a temporary file, added to the archive on close
// (tar headers need the size first).
class TarEntryWriteBackend : public FileBackend
{
public:
    TarEntryWriteBackend(uint64_t sessionId, const std::string& name);
    ~TarEntryWriteBackend() override;

protected:
    size_t
    produce(uint8_t* buffer, size_t capacity) override;
    void
    restart() override;
    void
    consume(const uint8_t* data, size_t size) override;
    void
    finish() override;

private:
    uint64_t sessionId;
    std::string name;
    std::wstring spoolPath;
    std::ofstream spool;
};
//=============================================================================
// Decompresses Zstandard or gzip data from a file or from bytes; seekable Zstandard data
// is read with random access.
class CompressedReadBackend : public FileBackend
{
public:
    CompressedReadBackend(StreamCodec codec, const std::wstring& filename,
        std::vector<uint8_t> bytes, uint64_t maxSize);
    ~CompressedReadBackend() override;

protected:
    size_t
    produce(uint8_t* buffer, size_t capacity) override;
    void
    restart() override;
    void
    consume(const uint8_t* data, size_t size) override;
    void
    finish() override;
    int64
    knownSize() const override;
    // Seekable data: restarts at the frame holding position.
    int64
    restartNear(int64 position) override;

private:
    StreamCodec codec;
    std::wstring filename;
    std::vector<uint8_t> bytes;
    size_t offset = 0;
    std::ifstream file;
    std::unique_ptr<ArchiveStreams::StreamDecoder> decoder;
    bool seekable = false;
    ZstdCodec::SeekTable seekTable;

    // Moves the compressed input to offset.
    void
    seekInput(uint64_t position);
    size_t
    input(uint8_t* buffer, size_t capacity);
};
//=============================================================================
// Compresses into a file (temporary file renamed on finish), or into memory when filename is
// empty.
class CompressedWriteBackend : public FileBackend
{
public:
    // append adds a new stream at the end of an existing file of the same format.
    // frameSize > 0 (Zstandard) writes the seekable format: frames and a seek table.
    CompressedWriteBackend(StreamCodec codec, const std::wstring& filename, int level,
        int workers = 0, bool append = false, uint32_t frameSize = 0);
    ~CompressedWriteBackend() override;
    bool
    isInMemory() const;
    // Compressed bytes of an in-memory writer, complete after close().
    const std::vector<uint8_t>&
    memoryBytes() const;

protected:
    size_t
    produce(uint8_t* buffer, size_t capacity) override;
    void
    restart() override;
    void
    consume(const uint8_t* data, size_t size) override;
    void
    finish() override;

private:
    StreamCodec codec;
    std::wstring filename;
    std::wstring temporary;
    std::ofstream file;
    std::vector<uint8_t> bytes;
    std::unique_ptr<ArchiveStreams::StreamEncoder> encoder;
    int level = 0;
    int workers = 0;
    uint32_t frameSize = 0;
    uint32_t frameInput = 0;
    uint64_t frameOutput = 0;
    std::vector<std::pair<uint32_t, uint32_t>> frames;

    void
    output(const uint8_t* data, size_t size);
    void
    startFrame();
    void
    endFrame();
    void
    discardTemporary() noexcept;
};
//=============================================================================
} // namespace Nelson::FileArchiverGateway
//=============================================================================
