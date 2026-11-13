//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cmath>
#include <fstream>
#include "streamBuiltinHelpers.hpp"
#include "ArchivePath.hpp"
#include "GzipCodec.hpp"
#include "ZstdCodec.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "NelsonConfiguration.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson::FileArchiverGateway {
//=============================================================================
StreamCodec
streamCodec(const ArrayOf& name)
{
    const std::wstring value = name.asWideString();
    return value == L"gzip" ? StreamCodec::Gzip : StreamCodec::Zstd;
}
//=============================================================================
void
raiseStreamError(
    const ArchiveStreams::StreamError& error, StreamCodec codec, const std::wstring& path)
{
    // Identifiers stay literal so that the message catalogs can be checked.
    const bool zstd = codec == StreamCodec::Zstd;
    const std::wstring detail = utf8_to_wstring(error.what());
    switch (error.code()) {
    case ArchiveStreams::StreamErrorCode::NotAvailable:
        Error(L"Nelson:io:zstd:notAvailable", L"Zstandard is not available in this build.");
        break;
    case ArchiveStreams::StreamErrorCode::InvalidLevel:
        if (zstd) {
            Error(L"Nelson:io:zstd:invalidLevel", L"Level must be an integer from %d to %d.",
                ZstdCodec::minimumLevel(), ZstdCodec::maximumLevel());
        }
        Error(L"Nelson:io:gzip:invalidLevel", L"Level must be an integer from %d to %d.",
            GzipCodec::minimumLevel(), GzipCodec::maximumLevel());
        break;
    case ArchiveStreams::StreamErrorCode::InvalidData:
        if (zstd) {
            Error(L"Nelson:io:zstd:invalidData", L"Not valid Zstandard data: %s", detail);
        }
        Error(L"Nelson:io:gzip:invalidData", L"Not valid gzip data: %s", detail);
        break;
    case ArchiveStreams::StreamErrorCode::TooLarge:
        if (zstd) {
            Error(L"Nelson:io:zstd:tooLarge", L"Decompressed data exceeds MaxSize.");
        }
        Error(L"Nelson:io:gzip:tooLarge", L"Decompressed data exceeds MaxSize.");
        break;
    case ArchiveStreams::StreamErrorCode::ReadFailed:
        if (zstd) {
            Error(L"Nelson:io:zstd:cannotRead", L"Cannot read file: %s", path);
        }
        Error(L"Nelson:io:gzip:cannotRead", L"Cannot read file: %s", path);
        break;
    case ArchiveStreams::StreamErrorCode::WriteFailed:
    default:
        if (zstd) {
            Error(L"Nelson:io:zstd:cannotWrite", L"Cannot write file: %s", path);
        }
        Error(L"Nelson:io:gzip:cannotWrite", L"Cannot write file: %s", path);
        break;
    }
    throw error;
}
//=============================================================================
int
zstdWorkers(const ArrayOf& value, uint64_t size)
{
    constexpr uint64_t automaticThreshold = 4 * 1024 * 1024;
    const double workers = value.asDoubleScalar();
    if (workers == -1) {
        if (size < automaticThreshold) {
            return 0;
        }
        const int threads = NelsonConfiguration::getInstance()->maxNumCompThreads();
        return threads > 1 ? threads : 0;
    }
    if (!(workers >= 0) || workers > 256 || workers != std::floor(workers)) {
        Error(L"Nelson:io:zstd:invalidWorkers",
            L"Workers must be 'auto' or an integer from 0 to 256.");
    }
    return static_cast<int>(workers);
}
//=============================================================================
std::unique_ptr<ArchiveStreams::StreamDecoder>
makeDecoder(StreamCodec codec, ArchiveStreams::Input input, uint64_t maxSize)
{
    if (codec == StreamCodec::Gzip) {
        return std::make_unique<GzipCodec::Decoder>(std::move(input), maxSize);
    }
    return std::make_unique<ZstdCodec::Decoder>(std::move(input), maxSize);
}
//=============================================================================
std::unique_ptr<ArchiveStreams::StreamEncoder>
makeEncoder(StreamCodec codec, ArchiveStreams::Output output, int level, int workers)
{
    if (codec == StreamCodec::Gzip) {
        return std::make_unique<GzipCodec::Encoder>(std::move(output), level);
    }
    return std::make_unique<ZstdCodec::Encoder>(std::move(output), level, workers);
}
//=============================================================================
bool
startsLikeCodecFile(StreamCodec codec, const std::wstring& filename)
{
    std::ifstream in(toNativePath(filename), std::ios::binary);
    uint8_t magic[4] = { 0, 0, 0, 0 };
    if (!in.is_open() || !in.read(reinterpret_cast<char*>(magic), 4)) {
        return !in.is_open() || in.gcount() == 0;
    }
    if (codec == StreamCodec::Gzip) {
        return magic[0] == 0x1F && magic[1] == 0x8B;
    }
    const uint32_t value = static_cast<uint32_t>(magic[0]) | (static_cast<uint32_t>(magic[1]) << 8)
        | (static_cast<uint32_t>(magic[2]) << 16) | (static_cast<uint32_t>(magic[3]) << 24);
    return value == 0xFD2FB528U || (value & 0xFFFFFFF0U) == 0x184D2A50U;
}
//=============================================================================
} // namespace Nelson::FileArchiverGateway
//=============================================================================
