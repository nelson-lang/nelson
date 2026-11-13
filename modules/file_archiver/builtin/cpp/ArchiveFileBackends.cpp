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
#include <filesystem>
#include "ArchiveFileBackends.hpp"
#include "ArchivePath.hpp"
#include "FileSystemWrapper.hpp"
#include "TarArchiveSessions.hpp"
#include "ZipArchiveSessions.hpp"
#include "tarBuiltinHelpers.hpp"
#include "zipBuiltinHelpers.hpp"
#include "streamBuiltinHelpers.hpp"
//=============================================================================
namespace Nelson::FileArchiverGateway {
//=============================================================================
ZipEntryReadBackend::ZipEntryReadBackend(ZipArchive archive, size_t index, uint64_t maxSize)
    : FileBackend(false), archive(std::move(archive)), index(index), maxSize(maxSize)
{
    this->archive.openEntry(index, maxSize);
}
//=============================================================================
ZipEntryReadBackend::~ZipEntryReadBackend() { closeQuietly(); }
//=============================================================================
size_t
ZipEntryReadBackend::produce(uint8_t* buffer, size_t capacity)
{
    try {
        const size_t count = archive.readEntryData(buffer, capacity);
        if (count == 0) {
            // End of the entry: the CRC is checked here.
            archive.closeEntry();
        }
        return count;
    } catch (const ZipArchiveError& error) {
        raiseZipError(error);
    }
}
//=============================================================================
void
ZipEntryReadBackend::restart()
{
    try {
        archive.openEntry(index, maxSize);
    } catch (const ZipArchiveError& error) {
        raiseZipError(error);
    }
}
//=============================================================================
void
ZipEntryReadBackend::consume(const uint8_t*, size_t)
{
}
//=============================================================================
void
ZipEntryReadBackend::finish()
{
}
//=============================================================================
int64
ZipEntryReadBackend::knownSize() const
{
    return static_cast<int64>(archive.entries()[index].size);
}
//=============================================================================
ZipEntryWriteBackend::ZipEntryWriteBackend(uint64_t sessionId)
    : FileBackend(true), sessionId(sessionId)
{
}
//=============================================================================
ZipEntryWriteBackend::~ZipEntryWriteBackend() { closeQuietly(); }
//=============================================================================
size_t
ZipEntryWriteBackend::produce(uint8_t*, size_t)
{
    return 0;
}
//=============================================================================
void
ZipEntryWriteBackend::restart()
{
}
//=============================================================================
void
ZipEntryWriteBackend::consume(const uint8_t* data, size_t size)
{
    try {
        ZipArchiveSessions::get(sessionId).writeEntryData(data, size);
    } catch (const ZipArchiveError& error) {
        raiseZipError(error);
    }
}
//=============================================================================
void
ZipEntryWriteBackend::finish()
{
    try {
        ZipArchiveSessions::finishEntry(sessionId);
    } catch (const ZipArchiveError& error) {
        raiseZipError(error);
    }
}
//=============================================================================
TarEntryReadBackend::TarEntryReadBackend(TarArchive archive, size_t index, uint64_t maxSize)
    : FileBackend(false), archive(std::move(archive)), index(index), maxSize(maxSize)
{
    this->archive.openEntry(index, maxSize);
}
//=============================================================================
TarEntryReadBackend::~TarEntryReadBackend() { closeQuietly(); }
//=============================================================================
size_t
TarEntryReadBackend::produce(uint8_t* buffer, size_t capacity)
{
    try {
        return archive.readEntryData(buffer, capacity);
    } catch (const ZipArchiveError& error) {
        raiseTarError(error);
    }
}
//=============================================================================
void
TarEntryReadBackend::restart()
{
    try {
        archive.openEntry(index, maxSize);
    } catch (const ZipArchiveError& error) {
        raiseTarError(error);
    }
}
//=============================================================================
void
TarEntryReadBackend::consume(const uint8_t*, size_t)
{
}
//=============================================================================
void
TarEntryReadBackend::finish()
{
}
//=============================================================================
int64
TarEntryReadBackend::knownSize() const
{
    return static_cast<int64>(archive.entries()[index].size);
}
//=============================================================================
TarEntryWriteBackend::TarEntryWriteBackend(uint64_t sessionId, const std::string& name)
    : FileBackend(true), sessionId(sessionId), name(name)
{
    spoolPath = FileSystemWrapper::Path::unique_path().wstring();
    spool.open(toNativePath(spoolPath), std::ios::binary | std::ios::trunc);
    if (!spool.is_open()) {
        throw ZipArchiveError(ZipErrorCode::WriteFailed, "Cannot write tar entry.", name);
    }
}
//=============================================================================
TarEntryWriteBackend::~TarEntryWriteBackend()
{
    closeQuietly();
    if (spool.is_open()) {
        spool.close();
    }
    std::error_code ec;
    std::filesystem::remove(toNativePath(spoolPath), ec);
}
//=============================================================================
size_t
TarEntryWriteBackend::produce(uint8_t*, size_t)
{
    return 0;
}
//=============================================================================
void
TarEntryWriteBackend::restart()
{
}
//=============================================================================
void
TarEntryWriteBackend::consume(const uint8_t* data, size_t size)
{
    spool.write(reinterpret_cast<const char*>(data), static_cast<std::streamsize>(size));
    if (!spool) {
        raiseTarError(ZipArchiveError(ZipErrorCode::WriteFailed, "Cannot write tar entry.", name));
    }
}
//=============================================================================
void
TarEntryWriteBackend::finish()
{
    spool.close();
    try {
        TarArchiveSessions::finishEntry(sessionId, name, spoolPath);
    } catch (const ZipArchiveError& error) {
        raiseTarError(error);
    }
}
//=============================================================================
CompressedReadBackend::CompressedReadBackend(
    StreamCodec codec, const std::wstring& filename, std::vector<uint8_t> bytes, uint64_t maxSize)
    : FileBackend(false), codec(codec), filename(filename), bytes(std::move(bytes))
{
    if (!filename.empty()) {
        file.open(toNativePath(filename), std::ios::binary);
        if (!file.is_open()) {
            throw ArchiveStreams::StreamError(
                ArchiveStreams::StreamErrorCode::ReadFailed, "Cannot read file.");
        }
    }
    decoder = makeDecoder(
        codec, [this](uint8_t* buffer, size_t capacity) { return input(buffer, capacity); },
        maxSize);
    if (codec != StreamCodec::Zstd) {
        return;
    }
    uint64_t sourceSize = this->bytes.size();
    if (file.is_open()) {
        file.seekg(0, std::ios::end);
        sourceSize = static_cast<uint64_t>(file.tellg());
    }
    auto readAt = [this](uint64_t position, uint8_t* buffer, size_t count) {
        if (!file.is_open()) {
            std::memcpy(buffer, this->bytes.data() + position, count);
            return true;
        }
        file.clear();
        file.seekg(static_cast<std::streamoff>(position));
        return static_cast<bool>(file.read(reinterpret_cast<char*>(buffer), count));
    };
    seekable = ZstdCodec::readSeekTable(readAt, sourceSize, seekTable)
        && seekTable.decompressedOffsets.back() <= maxSize;
    seekInput(0);
}
//=============================================================================
CompressedReadBackend::~CompressedReadBackend() { closeQuietly(); }
//=============================================================================
size_t
CompressedReadBackend::produce(uint8_t* buffer, size_t capacity)
{
    try {
        return decoder->read(buffer, capacity);
    } catch (const ArchiveStreams::StreamError& error) {
        raiseStreamError(error, codec, filename);
    }
}
//=============================================================================
void
CompressedReadBackend::restart()
{
    seekInput(0);
    decoder->reset();
}
//=============================================================================
void
CompressedReadBackend::consume(const uint8_t*, size_t)
{
}
//=============================================================================
void
CompressedReadBackend::finish()
{
}
//=============================================================================
int64
CompressedReadBackend::knownSize() const
{
    return seekable ? static_cast<int64>(seekTable.decompressedOffsets.back()) : -1;
}
//=============================================================================
int64
CompressedReadBackend::restartNear(int64 position)
{
    if (!seekable) {
        return -1;
    }
    const auto& starts = seekTable.decompressedOffsets;
    const auto last = starts.end() - 1;
    auto frame = std::upper_bound(starts.begin(), last, static_cast<uint64_t>(position));
    const size_t index
        = frame == starts.begin() ? 0 : static_cast<size_t>(frame - starts.begin()) - 1;
    seekInput(seekTable.compressedOffsets[index]);
    decoder->reset();
    return static_cast<int64>(starts[index]);
}
//=============================================================================
void
CompressedReadBackend::seekInput(uint64_t position)
{
    offset = static_cast<size_t>(position);
    if (file.is_open()) {
        file.clear();
        file.seekg(static_cast<std::streamoff>(position));
    }
}
//=============================================================================
size_t
CompressedReadBackend::input(uint8_t* buffer, size_t capacity)
{
    if (!file.is_open()) {
        const size_t count = std::min(capacity, bytes.size() - offset);
        std::memcpy(buffer, bytes.data() + offset, count);
        offset += count;
        return count;
    }
    file.read(reinterpret_cast<char*>(buffer), static_cast<std::streamsize>(capacity));
    if (file.bad()) {
        throw ArchiveStreams::StreamError(
            ArchiveStreams::StreamErrorCode::ReadFailed, "Cannot read file.");
    }
    return static_cast<size_t>(file.gcount());
}
//=============================================================================
CompressedWriteBackend::CompressedWriteBackend(StreamCodec codec, const std::wstring& filename,
    int level, int workers, bool append, uint32_t frameSize)
    : FileBackend(true)
    , codec(codec)
    , filename(filename)
    , level(level)
    , workers(workers)
    , frameSize(frameSize)
{
    startFrame();
    if (append) {
        if (!startsLikeCodecFile(codec, filename)) {
            throw ArchiveStreams::StreamError(ArchiveStreams::StreamErrorCode::InvalidData,
                "The file does not hold data of this format.");
        }
        file.open(toNativePath(filename), std::ios::binary | std::ios::app);
        if (!file.is_open()) {
            throw ArchiveStreams::StreamError(
                ArchiveStreams::StreamErrorCode::WriteFailed, "Cannot write file.");
        }
    } else if (!filename.empty()) {
        temporary = filename + L".nelson-tmp";
        file.open(toNativePath(temporary), std::ios::binary | std::ios::trunc);
        if (!file.is_open()) {
            throw ArchiveStreams::StreamError(
                ArchiveStreams::StreamErrorCode::WriteFailed, "Cannot write file.");
        }
    }
}
//=============================================================================
CompressedWriteBackend::~CompressedWriteBackend()
{
    closeQuietly();
    discardTemporary();
}
//=============================================================================
bool
CompressedWriteBackend::isInMemory() const
{
    return filename.empty();
}
//=============================================================================
const std::vector<uint8_t>&
CompressedWriteBackend::memoryBytes() const
{
    return bytes;
}
//=============================================================================
size_t
CompressedWriteBackend::produce(uint8_t*, size_t)
{
    return 0;
}
//=============================================================================
void
CompressedWriteBackend::restart()
{
}
//=============================================================================
void
CompressedWriteBackend::consume(const uint8_t* data, size_t size)
{
    try {
        while (size > 0) {
            if (!encoder) {
                startFrame();
            }
            const size_t chunk
                = frameSize == 0 ? size : std::min<size_t>(size, frameSize - frameInput);
            encoder->write(data, chunk);
            frameInput += static_cast<uint32_t>(frameSize == 0 ? 0 : chunk);
            data += chunk;
            size -= chunk;
            if (frameSize != 0 && frameInput == frameSize) {
                endFrame();
            }
        }
    } catch (const ArchiveStreams::StreamError& error) {
        raiseStreamError(error, codec, filename);
    }
}
//=============================================================================
void
CompressedWriteBackend::finish()
{
    try {
        if (frameSize == 0) {
            encoder->finish();
        } else {
            if (encoder || frames.empty()) {
                if (!encoder) {
                    startFrame();
                }
                endFrame();
            }
            const std::vector<uint8_t> table = ZstdCodec::seekTableFrame(frames);
            output(table.data(), table.size());
        }
        if (file.is_open()) {
            file.close();
            if (!file) {
                throw ArchiveStreams::StreamError(
                    ArchiveStreams::StreamErrorCode::WriteFailed, "Cannot write file.");
            }
            if (!temporary.empty()) {
                std::error_code ec;
                std::filesystem::rename(toNativePath(temporary), toNativePath(filename), ec);
                if (ec) {
                    throw ArchiveStreams::StreamError(
                        ArchiveStreams::StreamErrorCode::WriteFailed, "Cannot write file.");
                }
                temporary.clear();
            }
        }
    } catch (const ArchiveStreams::StreamError& error) {
        discardTemporary();
        raiseStreamError(error, codec, filename);
    }
}
//=============================================================================
void
CompressedWriteBackend::startFrame()
{
    encoder = makeEncoder(
        codec, [this](const uint8_t* data, size_t size) { output(data, size); }, level, workers);
    frameInput = 0;
    frameOutput = 0;
}
//=============================================================================
void
CompressedWriteBackend::endFrame()
{
    encoder->finish();
    encoder.reset();
    frames.emplace_back(static_cast<uint32_t>(frameOutput), frameInput);
}
//=============================================================================
void
CompressedWriteBackend::output(const uint8_t* data, size_t size)
{
    frameOutput += size;
    if (!file.is_open()) {
        bytes.insert(bytes.end(), data, data + size);
        return;
    }
    file.write(reinterpret_cast<const char*>(data), static_cast<std::streamsize>(size));
    if (!file) {
        throw ArchiveStreams::StreamError(
            ArchiveStreams::StreamErrorCode::WriteFailed, "Cannot write file.");
    }
}
//=============================================================================
void
CompressedWriteBackend::discardTemporary() noexcept
{
    if (temporary.empty()) {
        return;
    }
    if (file.is_open()) {
        file.close();
    }
    std::error_code ec;
    std::filesystem::remove(toNativePath(temporary), ec);
    temporary.clear();
}
//=============================================================================
} // namespace Nelson::FileArchiverGateway
//=============================================================================
