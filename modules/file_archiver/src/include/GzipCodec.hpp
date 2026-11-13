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
#include <memory>
#include <string>
#include <vector>
#include "ArchiveStreams.hpp"
#include "nlsFile_archiver_exports.h"
//=============================================================================
// gzip streams (RFC 1952) in memory and .gz files, through zlib.
namespace Nelson::GzipCodec {
//=============================================================================
using GzipErrorCode = ArchiveStreams::StreamErrorCode;
using GzipError = ArchiveStreams::StreamError;
using ArchiveStreams::Input;
using ArchiveStreams::noSizeLimit;
using ArchiveStreams::Output;
//=============================================================================
NLSFILE_ARCHIVER_IMPEXP std::string
version();
NLSFILE_ARCHIVER_IMPEXP int
minimumLevel();
NLSFILE_ARCHIVER_IMPEXP int
maximumLevel();
// -1 selects the zlib default level.
NLSFILE_ARCHIVER_IMPEXP std::vector<uint8_t>
compress(const uint8_t* data, size_t size, int level);
// All members of data, at most maxSize bytes of output.
NLSFILE_ARCHIVER_IMPEXP std::vector<uint8_t>
decompress(const uint8_t* data, size_t size, uint64_t maxSize);
NLSFILE_ARCHIVER_IMPEXP void
compressFile(const std::wstring& source, const std::wstring& destination, int level);
NLSFILE_ARCHIVER_IMPEXP void
decompressFile(const std::wstring& source, const std::wstring& destination, uint64_t maxSize);
//=============================================================================
// Pulls decompressed bytes from all members of input; rejects empty, truncated or
// oversized data.
class NLSFILE_ARCHIVER_IMPEXP Decoder : public ArchiveStreams::StreamDecoder
{
public:
    Decoder(Input input, uint64_t maxSize);
    ~Decoder() override;
    Decoder(const Decoder&) = delete;
    Decoder&
    operator=(const Decoder&)
        = delete;
    size_t
    read(uint8_t* buffer, size_t capacity) override;
    void
    reset() override;

private:
    struct Impl;
    std::unique_ptr<Impl> pImpl;
};
//=============================================================================
// Pushes data into one gzip member.
class NLSFILE_ARCHIVER_IMPEXP Encoder : public ArchiveStreams::StreamEncoder
{
public:
    Encoder(Output output, int level);
    ~Encoder() override;
    Encoder(const Encoder&) = delete;
    Encoder&
    operator=(const Encoder&)
        = delete;
    void
    write(const uint8_t* data, size_t size) override;
    void
    finish() override;

private:
    struct Impl;
    std::unique_ptr<Impl> pImpl;
};
//=============================================================================
} // namespace Nelson::GzipCodec
//=============================================================================
