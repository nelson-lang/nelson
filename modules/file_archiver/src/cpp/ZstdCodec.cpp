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
#include <functional>
#include <memory>
#include "ZstdCodec.hpp"
#include "ArchivePath.hpp"
#ifdef HAVE_ZSTD
#include <zstd.h>
#endif
//=============================================================================
namespace Nelson::ZstdCodec {
//=============================================================================
static constexpr size_t blockSize = 128 * 1024;
//=============================================================================
static void
requireAvailable();
static void
checkLevel(int level);
static void
compressStream(const Input& reader, const Output& writer, int level, int workers);
static uint32_t
readLittleEndian32(const uint8_t* bytes);
static void
appendLittleEndian32(std::vector<uint8_t>& bytes, uint32_t value);
//=============================================================================
constexpr uint32_t skippableSeekTableMagic = 0x184D2A5EU;
constexpr uint32_t seekableFooterMagic = 0x8F92EAB1U;
constexpr size_t seekTableFooterSize = 9;
//=============================================================================
std::string
version()
{
#ifdef HAVE_ZSTD
    return ZSTD_versionString();
#else
    return "";
#endif
}
//=============================================================================
int
minimumLevel()
{
#ifdef HAVE_ZSTD
    return ZSTD_minCLevel();
#else
    return 0;
#endif
}
//=============================================================================
int
maximumLevel()
{
#ifdef HAVE_ZSTD
    return ZSTD_maxCLevel();
#else
    return 0;
#endif
}
//=============================================================================
int
defaultLevel()
{
#ifdef HAVE_ZSTD
    return ZSTD_defaultCLevel();
#else
    return 0;
#endif
}
//=============================================================================
std::vector<uint8_t>
compress(const uint8_t* data, size_t size, int level, int workers)
{
    std::vector<uint8_t> result;
    Encoder encoder([&](const uint8_t* chunk,
                        size_t count) { result.insert(result.end(), chunk, chunk + count); },
        level, workers);
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
compressFile(const std::wstring& source, const std::wstring& destination, int level, int workers)
{
    requireAvailable();
    checkLevel(level);
    ArchiveStreams::transformFile(
        source, destination, [level, workers](const Input& reader, const Output& writer) {
            compressStream(reader, writer, level, workers);
        });
}
//=============================================================================
void
decompressFile(const std::wstring& source, const std::wstring& destination, uint64_t maxSize)
{
    requireAvailable();
    ArchiveStreams::transformFile(
        source, destination, [maxSize](const Input& reader, const Output& writer) {
            Decoder decoder(reader, maxSize);
            ArchiveStreams::decodeAll(decoder, writer);
        });
}
//=============================================================================
bool
readSeekTable(const ReadAt& readAt, uint64_t sourceSize, SeekTable& table)
{
    uint8_t footer[seekTableFooterSize];
    if (sourceSize < seekTableFooterSize + 8
        || !readAt(sourceSize - seekTableFooterSize, footer, seekTableFooterSize)
        || readLittleEndian32(footer + 5) != seekableFooterMagic || (footer[4] & 0x7C) != 0) {
        return false;
    }
    const uint64_t frames = readLittleEndian32(footer);
    const uint64_t entrySize = (footer[4] & 0x80) != 0 ? 12 : 8;
    const uint64_t frameContent = frames * entrySize + seekTableFooterSize;
    if (frames == 0 || frameContent + 8 > sourceSize) {
        return false;
    }
    std::vector<uint8_t> raw(static_cast<size_t>(frameContent + 8));
    if (!readAt(sourceSize - raw.size(), raw.data(), raw.size())
        || readLittleEndian32(raw.data()) != skippableSeekTableMagic
        || readLittleEndian32(raw.data() + 4) != frameContent) {
        return false;
    }
    table.compressedOffsets.assign(1, 0);
    table.decompressedOffsets.assign(1, 0);
    for (uint64_t k = 0; k < frames; ++k) {
        const uint8_t* entry = raw.data() + 8 + k * entrySize;
        table.compressedOffsets.push_back(
            table.compressedOffsets.back() + readLittleEndian32(entry));
        table.decompressedOffsets.push_back(
            table.decompressedOffsets.back() + readLittleEndian32(entry + 4));
    }
    // The frames must exactly fill the data before the seek table.
    return table.compressedOffsets.back() == sourceSize - raw.size();
}
//=============================================================================
std::vector<uint8_t>
seekTableFrame(const std::vector<std::pair<uint32_t, uint32_t>>& frames)
{
    std::vector<uint8_t> bytes;
    appendLittleEndian32(bytes, skippableSeekTableMagic);
    appendLittleEndian32(bytes, static_cast<uint32_t>(frames.size() * 8 + seekTableFooterSize));
    for (const auto& frame : frames) {
        appendLittleEndian32(bytes, frame.first);
        appendLittleEndian32(bytes, frame.second);
    }
    appendLittleEndian32(bytes, static_cast<uint32_t>(frames.size()));
    bytes.push_back(0);
    appendLittleEndian32(bytes, seekableFooterMagic);
    return bytes;
}
//=============================================================================
#ifdef HAVE_ZSTD
using DecoderContext = std::unique_ptr<ZSTD_DCtx, size_t (*)(ZSTD_DCtx*)>;
using EncoderContext = std::unique_ptr<ZSTD_CCtx, size_t (*)(ZSTD_CCtx*)>;
#endif
//=============================================================================
struct Decoder::Impl
{
    Input input;
    uint64_t maxSize = noSizeLimit;
#ifdef HAVE_ZSTD
    DecoderContext context { nullptr, ZSTD_freeDCtx };
    ZSTD_inBuffer in = { nullptr, 0, 0 };
#endif
    std::vector<uint8_t> inputBuffer;
    uint64_t produced = 0;
    size_t pending = 0;
    bool anyInput = false;
    bool inputEnded = false;
    bool outputWasFull = false;
    bool finished = false;
};
//=============================================================================
Decoder::Decoder(Input input, uint64_t maxSize) : pImpl(std::make_unique<Impl>())
{
    requireAvailable();
    pImpl->input = std::move(input);
    pImpl->maxSize = maxSize;
#ifdef HAVE_ZSTD
    pImpl->context.reset(ZSTD_createDCtx());
    if (!pImpl->context) {
        throw ZstdError(ZstdErrorCode::ReadFailed, "Cannot allocate a Zstandard context.");
    }
    pImpl->inputBuffer.resize(ZSTD_DStreamInSize());
    pImpl->in = { pImpl->inputBuffer.data(), 0, 0 };
#endif
}
//=============================================================================
Decoder::~Decoder() = default;
//=============================================================================
size_t
Decoder::read(uint8_t* buffer, size_t capacity)
{
#ifdef HAVE_ZSTD
    Impl& d = *pImpl;
    if (d.finished || capacity == 0) {
        return 0;
    }
    for (;;) {
        if (d.in.pos == d.in.size && !d.inputEnded) {
            const size_t count = d.input(d.inputBuffer.data(), d.inputBuffer.size());
            d.inputEnded = count == 0;
            d.anyInput = d.anyInput || count > 0;
            d.in = { d.inputBuffer.data(), count, 0 };
        }
        // Input consumed and nothing left inside the decoder: end of the data.
        if (d.in.pos == d.in.size && d.inputEnded && !d.outputWasFull) {
            d.finished = true;
            if (!d.anyInput || d.pending != 0) {
                throw ZstdError(ZstdErrorCode::InvalidData, "truncated or empty data");
            }
            return 0;
        }
        ZSTD_outBuffer out = { buffer, capacity, 0 };
        d.pending = ZSTD_decompressStream(d.context.get(), &out, &d.in);
        if (ZSTD_isError(d.pending)) {
            throw ZstdError(ZstdErrorCode::InvalidData, ZSTD_getErrorName(d.pending));
        }
        d.outputWasFull = out.pos == out.size;
        if (out.pos > 0) {
            d.produced += out.pos;
            if (d.produced > d.maxSize) {
                throw ZstdError(
                    ZstdErrorCode::TooLarge, "Decompressed data exceeds the size limit.");
            }
            return out.pos;
        }
    }
#else
    (void)buffer;
    (void)capacity;
    return 0;
#endif
}
//=============================================================================
void
Decoder::reset()
{
#ifdef HAVE_ZSTD
    ZSTD_DCtx_reset(pImpl->context.get(), ZSTD_reset_session_only);
    pImpl->in = { pImpl->inputBuffer.data(), 0, 0 };
#endif
    pImpl->produced = 0;
    pImpl->pending = 0;
    pImpl->anyInput = false;
    pImpl->inputEnded = false;
    pImpl->outputWasFull = false;
    pImpl->finished = false;
}
//=============================================================================
struct Encoder::Impl
{
    Output output;
#ifdef HAVE_ZSTD
    EncoderContext context { nullptr, ZSTD_freeCCtx };
#endif
    std::vector<uint8_t> outputBuffer;
    bool finished = false;
};
//=============================================================================
Encoder::Encoder(Output output, int level, int workers) : pImpl(std::make_unique<Impl>())
{
    requireAvailable();
    checkLevel(level);
    pImpl->output = std::move(output);
#ifdef HAVE_ZSTD
    pImpl->context.reset(ZSTD_createCCtx());
    if (!pImpl->context) {
        throw ZstdError(ZstdErrorCode::WriteFailed, "Cannot allocate a Zstandard context.");
    }
    ZSTD_CCtx_setParameter(pImpl->context.get(), ZSTD_c_compressionLevel, level);
    ZSTD_CCtx_setParameter(pImpl->context.get(), ZSTD_c_checksumFlag, 1);
    if (workers > 0) {
        // Ignored by a library built without threads.
        ZSTD_CCtx_setParameter(pImpl->context.get(), ZSTD_c_nbWorkers, workers);
    }
    pImpl->outputBuffer.resize(ZSTD_CStreamOutSize());
#else
    (void)workers;
#endif
}
//=============================================================================
Encoder::~Encoder() = default;
//=============================================================================
void
Encoder::write(const uint8_t* data, size_t size)
{
#ifdef HAVE_ZSTD
    if (pImpl->finished) {
        throw ZstdError(ZstdErrorCode::WriteFailed, "Zstandard frame already finished.");
    }
    ZSTD_inBuffer in = { data, size, 0 };
    while (in.pos < in.size) {
        ZSTD_outBuffer out = { pImpl->outputBuffer.data(), pImpl->outputBuffer.size(), 0 };
        const size_t status
            = ZSTD_compressStream2(pImpl->context.get(), &out, &in, ZSTD_e_continue);
        if (ZSTD_isError(status)) {
            throw ZstdError(ZstdErrorCode::WriteFailed, ZSTD_getErrorName(status));
        }
        pImpl->output(pImpl->outputBuffer.data(), out.pos);
    }
#else
    (void)data;
    (void)size;
#endif
}
//=============================================================================
void
Encoder::finish()
{
#ifdef HAVE_ZSTD
    if (pImpl->finished) {
        return;
    }
    pImpl->finished = true;
    ZSTD_inBuffer in = { nullptr, 0, 0 };
    size_t remaining = 0;
    do {
        ZSTD_outBuffer out = { pImpl->outputBuffer.data(), pImpl->outputBuffer.size(), 0 };
        remaining = ZSTD_compressStream2(pImpl->context.get(), &out, &in, ZSTD_e_end);
        if (ZSTD_isError(remaining)) {
            throw ZstdError(ZstdErrorCode::WriteFailed, ZSTD_getErrorName(remaining));
        }
        pImpl->output(pImpl->outputBuffer.data(), out.pos);
    } while (remaining != 0);
#endif
}
//=============================================================================
static void
requireAvailable()
{
#ifndef HAVE_ZSTD
    throw ZstdError(ZstdErrorCode::NotAvailable, "Zstandard is not available in this build.");
#endif
}
//=============================================================================
static void
checkLevel(int level)
{
    if (level != 0 && (level < minimumLevel() || level > maximumLevel())) {
        throw ZstdError(ZstdErrorCode::InvalidLevel, "Invalid Zstandard compression level.");
    }
}
//=============================================================================
// One frame with a content checksum, fed block by block.
static void
compressStream(const Input& reader, const Output& writer, int level, int workers)
{
    Encoder encoder(writer, level, workers);
    std::vector<uint8_t> block(workers > 0 ? 8 * blockSize : blockSize);
    for (size_t count = reader(block.data(), block.size()); count > 0;
        count = reader(block.data(), block.size())) {
        encoder.write(block.data(), count);
    }
    encoder.finish();
}
//=============================================================================
static uint32_t
readLittleEndian32(const uint8_t* bytes)
{
    return static_cast<uint32_t>(bytes[0]) | (static_cast<uint32_t>(bytes[1]) << 8)
        | (static_cast<uint32_t>(bytes[2]) << 16) | (static_cast<uint32_t>(bytes[3]) << 24);
}
//=============================================================================
static void
appendLittleEndian32(std::vector<uint8_t>& bytes, uint32_t value)
{
    for (int shift = 0; shift < 32; shift += 8) {
        bytes.push_back(static_cast<uint8_t>(value >> shift));
    }
}
//=============================================================================
} // namespace Nelson::ZstdCodec
//=============================================================================
