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
#if _MSC_VER
#pragma warning(disable : 4251)
#endif
//=============================================================================
#include <cstdint>
#include <functional>
#include <memory>
#include <stdexcept>
#include <string>
#include <vector>
#include "ArchiveStreams.hpp"
#include "nlsFile_archiver_exports.h"
//=============================================================================
// Zstandard frames in memory and .zst files, through libzstd when the build has it.
namespace Nelson::ZstdCodec {
//=============================================================================
using ZstdErrorCode = ArchiveStreams::StreamErrorCode;
using ZstdError = ArchiveStreams::StreamError;
using ArchiveStreams::noSizeLimit;
//=============================================================================
// Library version ("1.5.7"), empty when the build has no zstd.
NLSFILE_ARCHIVER_IMPEXP std::string
version();
NLSFILE_ARCHIVER_IMPEXP int
minimumLevel();
NLSFILE_ARCHIVER_IMPEXP int
maximumLevel();
NLSFILE_ARCHIVER_IMPEXP int
defaultLevel();
// One frame with a content checksum; workers > 0 compresses on that many threads.
NLSFILE_ARCHIVER_IMPEXP std::vector<uint8_t>
compress(const uint8_t* data, size_t size, int level, int workers = 0);
// All frames of data, at most maxSize bytes of output.
NLSFILE_ARCHIVER_IMPEXP std::vector<uint8_t>
decompress(const uint8_t* data, size_t size, uint64_t maxSize);
// Streams source into destination; destination is replaced only on success.
NLSFILE_ARCHIVER_IMPEXP void
compressFile(
    const std::wstring& source, const std::wstring& destination, int level, int workers = 0);
NLSFILE_ARCHIVER_IMPEXP void
decompressFile(const std::wstring& source, const std::wstring& destination, uint64_t maxSize);
//=============================================================================
// Seekable format: independent frames followed by a skippable frame indexing them.
struct SeekTable
{
    // Start of each frame in the compressed and decompressed data, plus the totals.
    std::vector<uint64_t> compressedOffsets;
    std::vector<uint64_t> decompressedOffsets;
};
// Reads count bytes at offset of the compressed source; false on failure.
using ReadAt = std::function<bool(uint64_t offset, uint8_t* buffer, size_t count)>;
// True and table filled when the source ends with a seek table (checksums are not used).
NLSFILE_ARCHIVER_IMPEXP bool
readSeekTable(const ReadAt& readAt, uint64_t sourceSize, SeekTable& table);
// Skippable frame indexing frames given as (compressed size, decompressed size).
NLSFILE_ARCHIVER_IMPEXP std::vector<uint8_t>
seekTableFrame(const std::vector<std::pair<uint32_t, uint32_t>>& frames);
//=============================================================================
using Input = ArchiveStreams::Input;
using Output = ArchiveStreams::Output;
//=============================================================================
// Pulls decompressed bytes from all frames of input; rejects empty, truncated or
// oversized data.
class NLSFILE_ARCHIVER_IMPEXP Decoder : public ArchiveStreams::StreamDecoder
{
public:
    Decoder(Input input, uint64_t maxSize);
    ~Decoder() override;
    Decoder(const Decoder&) = delete;
    Decoder&
    operator=(const Decoder&) = delete;
    // Up to capacity bytes, 0 at the end of the data.
    size_t
    read(uint8_t* buffer, size_t capacity) override;
    // Decodes again from the start; the caller restarts its input first.
    void
    reset() override;

private:
    struct Impl;
    std::unique_ptr<Impl> pImpl;
};
//=============================================================================
// Pushes data into one frame with a content checksum, written to output.
class NLSFILE_ARCHIVER_IMPEXP Encoder : public ArchiveStreams::StreamEncoder
{
public:
    // workers > 0 uses threads when the library supports them (same frame format).
    Encoder(Output output, int level, int workers = 0);
    ~Encoder() override;
    Encoder(const Encoder&) = delete;
    Encoder&
    operator=(const Encoder&) = delete;
    void
    write(const uint8_t* data, size_t size) override;
    // Ends the frame; the encoder accepts no more data.
    void
    finish() override;

private:
    struct Impl;
    std::unique_ptr<Impl> pImpl;
};
//=============================================================================
} // namespace Nelson::ZstdCodec
//=============================================================================
