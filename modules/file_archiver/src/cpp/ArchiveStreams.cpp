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
#include <filesystem>
#include <fstream>
#include <memory>
#include <vector>
#include "ArchiveStreams.hpp"
#include "ArchivePath.hpp"
#include "ArchiveInterruption.hpp"
//=============================================================================
namespace Nelson::ArchiveStreams {
//=============================================================================
static constexpr size_t blockSize = 128 * 1024;
//=============================================================================
StreamDecoder::~StreamDecoder() = default;
//=============================================================================
StreamEncoder::~StreamEncoder() = default;
//=============================================================================
size_t
initialReservation(uint64_t declared, uint64_t maxSize)
{
    constexpr uint64_t largestReservation = 16 * 1024 * 1024;
    return static_cast<size_t>(std::min({ declared, maxSize, largestReservation }));
}
//=============================================================================
Input
memoryInput(const uint8_t* data, size_t size)
{
    auto offset = std::make_shared<size_t>(0);
    return [data, size, offset](uint8_t* buffer, size_t capacity) {
        const size_t count = std::min(capacity, size - *offset);
        std::copy(data + *offset, data + *offset + count, buffer);
        *offset += count;
        return count;
    };
}
//=============================================================================
void
encodeAll(const Input& input, StreamEncoder& encoder)
{
    std::vector<uint8_t> block(blockSize);
    for (size_t count = input(block.data(), block.size()); count > 0;
        count = input(block.data(), block.size())) {
        ArchiveInterruption::throwIfInterrupted();
        encoder.write(block.data(), count);
    }
    encoder.finish();
}
//=============================================================================
void
decodeAll(StreamDecoder& decoder, const Output& output)
{
    std::vector<uint8_t> block(blockSize);
    for (size_t count = decoder.read(block.data(), block.size()); count > 0;
        count = decoder.read(block.data(), block.size())) {
        ArchiveInterruption::throwIfInterrupted();
        output(block.data(), count);
    }
}
//=============================================================================
void
transformFile(const std::wstring& source, const std::wstring& destination,
    const std::function<void(const Input&, const Output&)>& transform)
{
    namespace fs = std::filesystem;
    const fs::path from = toNativePath(source);
    const fs::path to = toNativePath(destination);
    std::error_code ec;
    if (fs::exists(to, ec) && fs::equivalent(from, to, ec)) {
        throw StreamError(
            StreamErrorCode::WriteFailed, "Source and destination are the same file.");
    }
    std::ifstream input(from, std::ios::binary);
    if (!input.is_open()) {
        throw StreamError(StreamErrorCode::ReadFailed, "Cannot read file.");
    }
    fs::path temporary = to;
    temporary += L".nelson-tmp";
    {
        std::ofstream output(temporary, std::ios::binary | std::ios::trunc);
        if (!output.is_open()) {
            throw StreamError(StreamErrorCode::WriteFailed, "Cannot write file.");
        }
        try {
            transform(
                [&](uint8_t* buffer, size_t size) {
                    input.read(reinterpret_cast<char*>(buffer), static_cast<std::streamsize>(size));
                    if (input.bad()) {
                        throw StreamError(StreamErrorCode::ReadFailed, "Cannot read file.");
                    }
                    return static_cast<size_t>(input.gcount());
                },
                [&](const uint8_t* data, size_t size) {
                    output.write(
                        reinterpret_cast<const char*>(data), static_cast<std::streamsize>(size));
                    if (!output) {
                        throw StreamError(StreamErrorCode::WriteFailed, "Cannot write file.");
                    }
                });
            output.close();
            if (!output) {
                throw StreamError(StreamErrorCode::WriteFailed, "Cannot write file.");
            }
        } catch (...) {
            output.close();
            fs::remove(temporary, ec);
            throw;
        }
    }
    fs::rename(temporary, to, ec);
    if (ec) {
        fs::remove(temporary, ec);
        throw StreamError(StreamErrorCode::WriteFailed, "Cannot write file.");
    }
}
//=============================================================================
} // namespace Nelson::ArchiveStreams
//=============================================================================
