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
#include <climits>
#include <zlib.h>
#include "GzipCodec.hpp"
//=============================================================================
namespace Nelson::GzipCodec {
//=============================================================================
static constexpr size_t blockSize = 128 * 1024;
// windowBits for deflate/inflate: 15 plus 16 selects the gzip wrapper.
static constexpr int gzipWindowBits = MAX_WBITS + 16;
//=============================================================================
static void
checkLevel(int level);
//=============================================================================
std::string
version()
{
    return zlibVersion();
}
//=============================================================================
int
minimumLevel()
{
    return 0;
}
//=============================================================================
int
maximumLevel()
{
    return 9;
}
//=============================================================================
std::vector<uint8_t>
compress(const uint8_t* data, size_t size, int level)
{
    std::vector<uint8_t> result;
    Encoder encoder([&](const uint8_t* chunk,
                        size_t count) { result.insert(result.end(), chunk, chunk + count); },
        level);
    encoder.write(data, size);
    encoder.finish();
    return result;
}
//=============================================================================
std::vector<uint8_t>
decompress(const uint8_t* data, size_t size, uint64_t maxSize)
{
    std::vector<uint8_t> result;
    Decoder decoder(ArchiveStreams::memoryInput(data, size), maxSize);
    ArchiveStreams::decodeAll(decoder, [&](const uint8_t* chunk, size_t count) {
        result.insert(result.end(), chunk, chunk + count);
    });
    return result;
}
//=============================================================================
void
compressFile(const std::wstring& source, const std::wstring& destination, int level)
{
    checkLevel(level);
    ArchiveStreams::transformFile(
        source, destination, [level](const Input& reader, const Output& writer) {
            Encoder encoder(writer, level);
            ArchiveStreams::encodeAll(reader, encoder);
        });
}
//=============================================================================
void
decompressFile(const std::wstring& source, const std::wstring& destination, uint64_t maxSize)
{
    ArchiveStreams::transformFile(
        source, destination, [maxSize](const Input& reader, const Output& writer) {
            Decoder decoder(reader, maxSize);
            ArchiveStreams::decodeAll(decoder, writer);
        });
}
//=============================================================================
struct Decoder::Impl
{
    Input input;
    uint64_t maxSize = noSizeLimit;
    z_stream stream = {};
    std::vector<uint8_t> inputBuffer;
    uint64_t produced = 0;
    bool anyInput = false;
    bool inputEnded = false;
    bool memberOpen = false;
    bool finished = false;

    ~Impl() { inflateEnd(&stream); }
};
//=============================================================================
Decoder::Decoder(Input input, uint64_t maxSize) : pImpl(std::make_unique<Impl>())
{
    pImpl->input = std::move(input);
    pImpl->maxSize = maxSize;
    pImpl->inputBuffer.resize(blockSize);
    if (inflateInit2(&pImpl->stream, gzipWindowBits) != Z_OK) {
        throw GzipError(GzipErrorCode::ReadFailed, "Cannot allocate a gzip stream.");
    }
}
//=============================================================================
Decoder::~Decoder() = default;
//=============================================================================
size_t
Decoder::read(uint8_t* buffer, size_t capacity)
{
    Impl& d = *pImpl;
    if (d.finished || capacity == 0) {
        return 0;
    }
    for (;;) {
        if (d.stream.avail_in == 0 && !d.inputEnded) {
            const size_t count = d.input(d.inputBuffer.data(), d.inputBuffer.size());
            d.inputEnded = count == 0;
            d.anyInput = d.anyInput || count > 0;
            d.stream.next_in = d.inputBuffer.data();
            d.stream.avail_in = static_cast<uInt>(count);
        }
        if (d.stream.avail_in == 0 && d.inputEnded) {
            d.finished = true;
            if (!d.anyInput || d.memberOpen) {
                throw GzipError(GzipErrorCode::InvalidData, "truncated or empty data");
            }
            return 0;
        }
        d.memberOpen = true;
        d.stream.next_out = buffer;
        d.stream.avail_out = static_cast<uInt>(std::min<size_t>(capacity, UINT_MAX));
        const uInt available = d.stream.avail_out;
        const int status = inflate(&d.stream, Z_NO_FLUSH);
        if (status == Z_STREAM_END) {
            // Another member may follow (concatenated gzip files).
            d.memberOpen = false;
            inflateReset(&d.stream);
        } else if (status != Z_OK && status != Z_BUF_ERROR) {
            throw GzipError(GzipErrorCode::InvalidData,
                d.stream.msg != nullptr ? d.stream.msg : "invalid data");
        }
        const size_t count = available - d.stream.avail_out;
        if (count > 0) {
            d.produced += count;
            if (d.produced > d.maxSize) {
                throw GzipError(
                    GzipErrorCode::TooLarge, "Decompressed data exceeds the size limit.");
            }
            return count;
        }
    }
}
//=============================================================================
void
Decoder::reset()
{
    Impl& d = *pImpl;
    inflateReset(&d.stream);
    d.stream.avail_in = 0;
    d.produced = 0;
    d.anyInput = false;
    d.inputEnded = false;
    d.memberOpen = false;
    d.finished = false;
}
//=============================================================================
struct Encoder::Impl
{
    Output output;
    z_stream stream = {};
    std::vector<uint8_t> outputBuffer;
    bool finished = false;

    ~Impl() { deflateEnd(&stream); }

    // Compresses the pending input with flush, sending every produced block.
    int
    pump(int flush)
    {
        int status = Z_OK;
        do {
            stream.next_out = outputBuffer.data();
            stream.avail_out = static_cast<uInt>(outputBuffer.size());
            status = deflate(&stream, flush);
            if (status == Z_STREAM_ERROR) {
                throw GzipError(GzipErrorCode::WriteFailed, "Cannot compress gzip data.");
            }
            const size_t count = outputBuffer.size() - stream.avail_out;
            if (count > 0) {
                output(outputBuffer.data(), count);
            }
        } while (stream.avail_out == 0 || (flush == Z_FINISH && status != Z_STREAM_END));
        return status;
    }
};
//=============================================================================
Encoder::Encoder(Output output, int level) : pImpl(std::make_unique<Impl>())
{
    checkLevel(level);
    pImpl->output = std::move(output);
    pImpl->outputBuffer.resize(blockSize);
    if (deflateInit2(&pImpl->stream, level, Z_DEFLATED, gzipWindowBits, 8, Z_DEFAULT_STRATEGY)
        != Z_OK) {
        throw GzipError(GzipErrorCode::WriteFailed, "Cannot allocate a gzip stream.");
    }
}
//=============================================================================
Encoder::~Encoder() = default;
//=============================================================================
void
Encoder::write(const uint8_t* data, size_t size)
{
    if (pImpl->finished) {
        throw GzipError(GzipErrorCode::WriteFailed, "gzip stream already finished.");
    }
    while (size > 0) {
        const auto chunk = static_cast<uInt>(std::min<size_t>(size, UINT_MAX));
        pImpl->stream.next_in = const_cast<Bytef*>(data);
        pImpl->stream.avail_in = chunk;
        pImpl->pump(Z_NO_FLUSH);
        data += chunk;
        size -= chunk;
    }
}
//=============================================================================
void
Encoder::finish()
{
    if (pImpl->finished) {
        return;
    }
    pImpl->finished = true;
    pImpl->stream.next_in = nullptr;
    pImpl->stream.avail_in = 0;
    pImpl->pump(Z_FINISH);
}
//=============================================================================
static void
checkLevel(int level)
{
    if (level != -1 && (level < minimumLevel() || level > maximumLevel())) {
        throw GzipError(GzipErrorCode::InvalidLevel, "Invalid gzip compression level.");
    }
}
//=============================================================================
} // namespace Nelson::GzipCodec
//=============================================================================
