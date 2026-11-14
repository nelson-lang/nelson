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
#include <cctype>
#include <filesystem>
#include <fstream>
#include <limits>
#include <unordered_map>
#include <sys/stat.h>
#include "LibarchiveSupport.hpp"
#include "ZipArchive.hpp"
#include "ArchiveInterruption.hpp"
#include "ArchiveStreams.hpp"
#include "ZipCentralDirectory.hpp"
#include "ArchivePath.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    constexpr size_t blockSize = 64 * 1024;
    constexpr int defaultLevel = -1;
    constexpr uint16_t unixMadeBy = 3 << 8;
    constexpr uint32_t localHeaderSignature = 0x04034b50;
    constexpr uint32_t maxLzmaDictionary = 64 * 1024 * 1024;

    uint16_t
    littleEndian16(const uint8_t* bytes)
    {
        return static_cast<uint16_t>(bytes[0] | (bytes[1] << 8));
    }

    uint32_t
    littleEndian32(const uint8_t* bytes)
    {
        return static_cast<uint32_t>(bytes[0]) | (static_cast<uint32_t>(bytes[1]) << 8)
            | (static_cast<uint32_t>(bytes[2]) << 16) | (static_cast<uint32_t>(bytes[3]) << 24);
    }
} // namespace
//=============================================================================
[[noreturn]] static void
fail(ZipErrorCode code, const std::string& message, const std::string& entry = "");
static std::vector<ZipDirectoryEntry>
indexFile(const std::wstring& filename);
static std::vector<ZipDirectoryEntry>
indexMemory(const std::vector<uint8_t>& bytes);
static int
setWriteCompression(struct archive* archive, ZipCompression method);
static bool
isWritableMethod(uint16_t method);
//=============================================================================
struct ZipArchive::Impl
{
    bool writing = false;
    bool open = false;
    bool entryOpen = false;
    std::vector<ZipEntryInfo> entries;
    std::unordered_map<std::string, size_t> index;
    std::vector<ZipDirectoryEntry> directory;
    // Reading: libarchive gives the entries in the order of their position in the archive.
    std::wstring sourceFile;
    std::shared_ptr<const std::vector<uint8_t>> memoryInput;
    std::vector<uint64_t> ranks;
    std::string password;
    struct archive* reader = nullptr;
    uint64_t headersRead = 0;
    size_t readIndex = 0;
    uint64_t readProduced = 0;
    std::vector<uint8_t> peeked;
    size_t peekedOffset = 0;
    // Writing: libarchive starts at the first file, whose level then applies to the archive.
    struct archive* writer = nullptr;
    std::wstring destination;
    std::wstring writeTarget;
    bool replaceOnClose = false;
    std::vector<uint8_t> memoryOutput;
    ZipEncryption encryption = ZipEncryption::None;
    std::string writePassword;
    int archiveLevel = defaultLevel;
    std::vector<ZipEntryInfo> pendingFolders;
    std::unique_ptr<ZipArchive> appendSource;
    ZipEntryInfo currentEntry;
    bool currentIsPending = false;

    ~Impl()
    {
        closeReader();
        if (writer != nullptr) {
            archive_write_free(writer);
            writer = nullptr;
        }
        if (replaceOnClose && open) {
            std::error_code ec;
            std::filesystem::remove(toNativePath(writeTarget), ec);
        }
    }

    // libarchive write callback of an in-memory archive.
    static la_ssize_t
    appendToMemory(struct archive* /*handle*/, void* client, const void* buffer, size_t length)
    {
        auto* impl = static_cast<Impl*>(client);
        const auto* bytes = static_cast<const uint8_t*>(buffer);
        impl->memoryOutput.insert(impl->memoryOutput.end(), bytes, bytes + length);
        return static_cast<la_ssize_t>(length);
    }

    void
    requireOpen(bool forWriting) const
    {
        if (!open) {
            fail(ZipErrorCode::InvalidState, "ZIP archive is closed.");
        }
        if (writing != forWriting) {
            fail(ZipErrorCode::InvalidState,
                forWriting ? "ZIP archive is open for reading."
                           : "ZIP archive is open for writing.");
        }
    }

    void
    setIndex(const std::vector<ZipDirectoryEntry>& newDirectory)
    {
        entries.clear();
        index.clear();
        directory = newDirectory;
        ranks.assign(directory.size(), 0);
        std::vector<size_t> order(directory.size());
        for (size_t k = 0; k < directory.size(); ++k) {
            order[k] = k;
            index[directory[k].info.name] = k;
            entries.push_back(directory[k].info);
        }
        std::sort(order.begin(), order.end(), [&](size_t a, size_t b) {
            return directory[a].localOffset < directory[b].localOffset;
        });
        for (size_t rank = 0; rank < order.size(); ++rank) {
            ranks[order[rank]] = rank;
        }
    }

    void
    closeReader()
    {
        if (reader != nullptr) {
            archive_read_free(reader);
            reader = nullptr;
        }
        headersRead = 0;
    }

    void
    openReader()
    {
        closeReader();
        reader = archive_read_new();
        if (reader == nullptr) {
            fail(ZipErrorCode::OpenFailed, "Cannot allocate ZIP reader.");
        }
        archive_read_support_format_zip_seekable(reader);
        // macOS resource forks stay ordinary entries.
        archive_read_set_options(reader, "zip:!mac-ext");
        if (!password.empty()) {
            archive_read_add_passphrase(reader, password.c_str());
        }
        int status = ARCHIVE_FATAL;
        if (memoryInput) {
            status = archive_read_open_memory(reader, memoryInput->data(), memoryInput->size());
        } else {
#ifdef _WIN32
            status = archive_read_open_filename_w(reader, sourceFile.c_str(), blockSize);
#else
            status = archive_read_open_filename(reader, wideToUtf8(sourceFile).c_str(), blockSize);
#endif
        }
        if (status != ARCHIVE_OK) {
            closeReader();
            fail(ZipErrorCode::OpenFailed, "Cannot open ZIP archive.");
        }
    }

    // Moves the reader to the entry of this rank (reopens it to go backward).
    void
    moveTo(uint64_t rank, const ZipEntryInfo& info)
    {
        if (reader == nullptr || headersRead > rank) {
            openReader();
        }
        struct archive_entry* item = nullptr;
        while (headersRead <= rank) {
            const int status = archive_read_next_header(reader, &item);
            if (status != ARCHIVE_OK && status != ARCHIVE_WARN) {
                fail(ZipErrorCode::ReadFailed, "Cannot open ZIP entry.", info.name);
            }
            ++headersRead;
        }
        if (archive_entry_size_is_set(item) != 0
            && static_cast<uint64_t>(archive_entry_size(item)) != info.size) {
            fail(ZipErrorCode::ReadFailed, "Cannot open ZIP entry.", info.name);
        }
    }

    size_t
    readData(void* buffer, size_t capacity)
    {
        const la_ssize_t count
            = archive_read_data(reader, buffer, std::min<size_t>(capacity, blockSize));
        if (count < 0) {
            fail(libarchiveReadErrorCode(reader), "Cannot read ZIP entry data.",
                entries[readIndex].name);
        }
        return static_cast<size_t>(count);
    }

    bool
    readSource(uint64_t offset, uint8_t* buffer, size_t size) const
    {
        if (memoryInput) {
            if (offset > memoryInput->size() || size > memoryInput->size() - offset) {
                return false;
            }
            std::copy_n(memoryInput->data() + offset, size, buffer);
            return true;
        }
        if (offset > static_cast<uint64_t>(std::numeric_limits<std::streamoff>::max())) {
            return false;
        }
        std::ifstream input(toNativePath(sourceFile), std::ios::binary);
        if (!input.is_open()) {
            return false;
        }
        input.seekg(static_cast<std::streamoff>(offset));
        input.read(reinterpret_cast<char*>(buffer), static_cast<std::streamsize>(size));
        return input.gcount() == static_cast<std::streamsize>(size);
    }

    void
    validateLzmaDictionary(size_t entryIndex) const
    {
        const ZipEntryInfo& info = entries[entryIndex];
        if (info.method != static_cast<uint16_t>(ZipCompression::Lzma) || info.isEncrypted) {
            return;
        }
        const uint64_t localOffset = directory[entryIndex].localOffset;
        uint8_t localHeader[30];
        if (!readSource(localOffset, localHeader, sizeof(localHeader))
            || littleEndian32(localHeader) != localHeaderSignature) {
            fail(ZipErrorCode::NotAZipArchive, "Invalid ZIP local header.", info.name);
        }
        const uint64_t dataOffset = localOffset + sizeof(localHeader)
            + littleEndian16(localHeader + 26) + littleEndian16(localHeader + 28);
        uint8_t lzmaHeader[9];
        if (dataOffset < localOffset || info.compressedSize < sizeof(lzmaHeader)
            || !readSource(dataOffset, lzmaHeader, sizeof(lzmaHeader))) {
            fail(ZipErrorCode::NotAZipArchive, "Invalid ZIP LZMA data.", info.name);
        }
        if (littleEndian16(lzmaHeader + 2) != 5) {
            fail(ZipErrorCode::NotAZipArchive, "Invalid ZIP LZMA data.", info.name);
        }
        if (littleEndian32(lzmaHeader + 5) > maxLzmaDictionary) {
            fail(ZipErrorCode::EntryTooLarge, "ZIP LZMA dictionary is too large.", info.name);
        }
    }

    // Starts libarchive with the level of the first file, then writes what waited.
    void
    startWriter(int level)
    {
        writer = archive_write_new();
        if (writer == nullptr) {
            fail(ZipErrorCode::OpenFailed, "Cannot allocate ZIP writer.");
        }
        archive_write_set_format_zip(writer);
        archive_write_set_options(writer, "zip:hdrcharset=UTF-8");
        archiveLevel = level;
        if (level >= 0) {
            const std::string value = std::to_string(level);
            if (archive_write_set_format_option(writer, "zip", "compression-level", value.c_str())
                != ARCHIVE_OK) {
                fail(ZipErrorCode::InvalidArgument, "Unsupported ZIP compression level.");
            }
        }
        if (encryption != ZipEncryption::None) {
            const char* name = encryption == ZipEncryption::ZipCrypto ? "zipcrypt"
                : encryption == ZipEncryption::Aes128                 ? "aes128"
                                                                      : "aes256";
            if (archive_write_set_format_option(writer, "zip", "encryption", name) != ARCHIVE_OK
                || archive_write_set_passphrase(writer, writePassword.c_str()) != ARCHIVE_OK) {
                fail(ZipErrorCode::InvalidArgument, "ZIP encryption is not available.");
            }
        }
        archive_write_set_bytes_per_block(writer, static_cast<int>(blockSize));
        archive_write_set_bytes_in_last_block(writer, 1);
        int status = ARCHIVE_FATAL;
        if (writeTarget.empty()) {
            status = archive_write_open2(writer, this, nullptr, appendToMemory, nullptr, nullptr);
        } else {
#ifdef _WIN32
            status = archive_write_open_filename_w(writer, writeTarget.c_str());
#else
            status = archive_write_open_filename(writer, wideToUtf8(writeTarget).c_str());
#endif
        }
        if (status != ARCHIVE_OK) {
            fail(ZipErrorCode::OpenFailed, "Cannot create ZIP archive.");
        }
        if (appendSource) {
            copyExistingEntries();
        }
        for (const ZipEntryInfo& folder : pendingFolders) {
            writeHeader(folder, 0);
            finishEntry(folder.name);
        }
        pendingFolders.clear();
    }

    // Keeps the level of the archive: libarchive sets it once for all entries.
    void
    useLevel(int level)
    {
        if (writer == nullptr) {
            startWriter(level);
            return;
        }
        if (level != defaultLevel && level != archiveLevel) {
            fail(ZipErrorCode::InvalidArgument,
                "All entries of a ZIP archive use the compression level of its first file.");
        }
    }

    void
    writeHeader(const ZipEntryInfo& info, int64_t size)
    {
        if (setWriteCompression(writer, static_cast<ZipCompression>(info.method)) != ARCHIVE_OK) {
            fail(ZipErrorCode::UnsupportedCompression, "Unsupported ZIP compression method.",
                info.name);
        }
        struct archive_entry* item = archive_entry_new();
        archive_entry_set_pathname_utf8(item, info.name.c_str());
        archive_entry_set_mtime(item, info.modified, 0);
        // Unix mode from the attributes, as fileMetadata gives them.
        const uint32_t mode = info.externalAttributes >> 16;
        unsigned int permissions = info.isDirectory ? 0755 : 0644;
        if ((mode & 0170000) != 0) {
            permissions = mode & 07777;
        }
        archive_entry_set_filetype(item, info.isDirectory ? AE_IFDIR : AE_IFREG);
        archive_entry_set_perm(item, permissions);
        if (info.isDirectory) {
            archive_entry_set_size(item, 0);
        } else if (size >= 0) {
            archive_entry_set_size(item, size);
        }
        const int status = archive_write_header(writer, item);
        archive_entry_free(item);
        if (status != ARCHIVE_OK) {
            fail(ZipErrorCode::WriteFailed, "Cannot add ZIP entry.", info.name);
        }
    }

    void
    writeData(const void* data, size_t size, const std::string& name)
    {
        const auto* bytes = static_cast<const uint8_t*>(data);
        while (size > 0) {
            const size_t chunk = std::min(size, blockSize);
            if (archive_write_data(writer, bytes, chunk) != static_cast<la_ssize_t>(chunk)) {
                fail(ZipErrorCode::WriteFailed, "Cannot write ZIP entry data.", name);
            }
            bytes += chunk;
            size -= chunk;
        }
    }

    void
    finishEntry(const std::string& name)
    {
        if (archive_write_finish_entry(writer) != ARCHIVE_OK) {
            fail(ZipErrorCode::WriteFailed, "Cannot write ZIP entry.", name);
        }
    }

    void
    record(const ZipEntryInfo& info)
    {
        index[info.name] = entries.size();
        entries.push_back(info);
    }

    // Append mode: the entries already there are written again first.
    void
    copyExistingEntries()
    {
        std::unique_ptr<ZipArchive> source = std::move(appendSource);
        for (size_t k = 0; k < source->entries().size(); ++k) {
            ZipEntryInfo info = source->entries()[k];
            info.isEncrypted = false;
            if (!isWritableMethod(info.method)) {
                info.method = static_cast<uint16_t>(ZipCompression::Deflate);
            }
            writeHeader(info, static_cast<int64_t>(info.size));
            if (!info.isDirectory) {
                source->readEntry(
                    k, [&](const uint8_t* data, size_t size) { writeData(data, size, info.name); });
            }
            finishEntry(info.name);
        }
        source->close();
    }
};
//=============================================================================
ZipArchive::ZipArchive(std::unique_ptr<Impl> impl) : pImpl(std::move(impl)) { }
//=============================================================================
ZipArchive::ZipArchive(ZipArchive&& other) noexcept = default;
//=============================================================================
ZipArchive&
ZipArchive::operator=(ZipArchive&& other) noexcept
    = default;
//=============================================================================
ZipArchive::~ZipArchive() = default;
//=============================================================================
ZipArchive
ZipArchive::openFile(const std::wstring& filename, Mode mode)
{
    auto impl = std::make_unique<Impl>();
    std::error_code ec;
    const bool exists = std::filesystem::exists(toNativePath(filename), ec);
    switch (mode) {
    case Mode::Read: {
        impl->sourceFile = filename;
        impl->setIndex(indexFile(filename));
    } break;
    case Mode::Create:
    case Mode::Overwrite: {
        if (exists && mode == Mode::Create) {
            fail(ZipErrorCode::OpenFailed, "File already exists.");
        }
        // Reports an unwritable destination now, not at the first entry.
        {
            std::ofstream probe(toNativePath(filename), std::ios::binary | std::ios::trunc);
            if (!probe.is_open()) {
                fail(ZipErrorCode::OpenFailed, "Cannot open file.");
            }
        }
        impl->writing = true;
        impl->destination = filename;
        impl->writeTarget = filename;
    } break;
    case Mode::Append: {
        if (!exists) {
            fail(ZipErrorCode::OpenFailed, "Cannot open file.");
        }
        auto existing = std::make_unique<ZipArchive>(openFile(filename, Mode::Read));
        for (const ZipEntryInfo& entry : existing->entries()) {
            if (entry.isEncrypted) {
                fail(ZipErrorCode::InvalidArgument,
                    "Entries cannot be added to an archive with encrypted entries: " + entry.name
                        + ".",
                    entry.name);
            }
        }
        // libarchive cannot append: the archive is written again into a temporary file.
        impl->entries = existing->entries();
        impl->index = existing->pImpl->index;
        impl->appendSource = std::move(existing);
        impl->writing = true;
        impl->destination = filename;
        impl->writeTarget = filename + L".nelson-append";
        impl->replaceOnClose = true;
    } break;
    }
    impl->open = true;
    return ZipArchive(std::move(impl));
}
//=============================================================================
ZipArchive
ZipArchive::openMemory(std::vector<uint8_t> bytes)
{
    auto impl = std::make_unique<Impl>();
    impl->memoryInput = std::make_shared<const std::vector<uint8_t>>(std::move(bytes));
    impl->setIndex(indexMemory(*impl->memoryInput));
    impl->open = true;
    return ZipArchive(std::move(impl));
}
//=============================================================================
ZipArchive
ZipArchive::createInMemory()
{
    auto impl = std::make_unique<Impl>();
    impl->writing = true;
    impl->open = true;
    return ZipArchive(std::move(impl));
}
//=============================================================================
bool
ZipArchive::supportsCompression(uint16_t method)
{
    return isWritableMethod(method);
}
//=============================================================================
std::string
ZipArchive::compressionName(uint16_t method)
{
    switch (static_cast<ZipCompression>(method)) {
    case ZipCompression::Store:
        return "store";
    case ZipCompression::Deflate:
        return "deflate";
    case ZipCompression::Bzip2:
        return "bzip2";
    case ZipCompression::Lzma:
        return "lzma";
    case ZipCompression::Zstd:
        return "zstd";
    case ZipCompression::Xz:
        return "xz";
    default:
        return "method " + std::to_string(method);
    }
}
//=============================================================================
bool
ZipArchive::compressionFromName(const std::string& name, ZipCompression& method)
{
    std::string lower = name;
    std::transform(lower.begin(), lower.end(), lower.begin(),
        [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
    for (ZipCompression candidate : availableCompressions()) {
        if (compressionName(static_cast<uint16_t>(candidate)) == lower) {
            method = candidate;
            return true;
        }
    }
    return false;
}
//=============================================================================
std::vector<ZipCompression>
ZipArchive::availableCompressions()
{
    // Methods this libarchive build can write (and so read), asked once.
    static const std::vector<ZipCompression> methods = []() {
        std::vector<ZipCompression> found;
        const ZipCompression candidates[] = {
            ZipCompression::Store,
            ZipCompression::Deflate,
#if ARCHIVE_VERSION_NUMBER >= 3008000
            ZipCompression::Zstd,
            ZipCompression::Xz,
            ZipCompression::Bzip2,
            ZipCompression::Lzma,
#endif
        };
        for (ZipCompression method : candidates) {
            struct archive* probe = archive_write_new();
            if (probe != nullptr && archive_write_set_format_zip(probe) == ARCHIVE_OK
                && setWriteCompression(probe, method) == ARCHIVE_OK) {
                found.push_back(method);
            }
            if (probe != nullptr) {
                archive_write_free(probe);
            }
        }
        return found;
    }();
    return methods;
}
//=============================================================================
const std::vector<ZipEntryInfo>&
ZipArchive::entries() const
{
    return pImpl->entries;
}
//=============================================================================
std::optional<size_t>
ZipArchive::find(const std::string& name) const
{
    auto it = pImpl->index.find(name);
    if (it == pImpl->index.end()) {
        return std::nullopt;
    }
    return it->second;
}
//=============================================================================
void
ZipArchive::readEntry(
    size_t index, const std::function<void(const uint8_t*, size_t)>& sink, uint64_t maxSize)
{
    openEntry(index, maxSize);
    // Leaves the entry when sink or a read throws.
    struct EntryGuard
    {
        Impl* impl;
        ~EntryGuard() { impl->entryOpen = false; }
    } guard { pImpl.get() };
    std::vector<uint8_t> block(blockSize);
    for (size_t count = readEntryData(block.data(), block.size()); count > 0;
        count = readEntryData(block.data(), block.size())) {
        sink(block.data(), count);
    }
    closeEntry();
}
//=============================================================================
void
ZipArchive::openEntry(size_t index, uint64_t maxSize)
{
    pImpl->requireOpen(false);
    if (index >= pImpl->entries.size()) {
        fail(ZipErrorCode::EntryNotFound, "ZIP entry index out of range.");
    }
    const ZipEntryInfo& info = pImpl->entries[index];
    if (info.isEncrypted && pImpl->password.empty()) {
        fail(ZipErrorCode::EncryptedEntry, "Encrypted ZIP entry needs a password.", info.name);
    }
    if (!supportsCompression(info.method)) {
        fail(
            ZipErrorCode::UnsupportedCompression, "Unsupported ZIP compression method.", info.name);
    }
    if (info.size > maxSize) {
        fail(ZipErrorCode::SizeLimitExceeded, "ZIP entry exceeds the size limit.", info.name);
    }
    pImpl->validateLzmaDictionary(index);
    pImpl->entryOpen = false;
    pImpl->moveTo(pImpl->ranks[index], info);
    pImpl->entryOpen = true;
    pImpl->readIndex = index;
    pImpl->readProduced = 0;
    pImpl->peeked.clear();
    pImpl->peekedOffset = 0;
    if (info.isEncrypted && info.size > 0) {
        // The password is checked by the first read: done now, so that opening reports it.
        pImpl->peeked.resize(blockSize);
        pImpl->peeked.resize(pImpl->readData(pImpl->peeked.data(), pImpl->peeked.size()));
    }
}
//=============================================================================
size_t
ZipArchive::readEntryData(void* buffer, size_t capacity)
{
    pImpl->requireOpen(false);
    if (!pImpl->entryOpen) {
        fail(ZipErrorCode::InvalidState, "No ZIP entry is open.");
    }
    const ZipEntryInfo& info = pImpl->entries[pImpl->readIndex];
    size_t count = 0;
    if (pImpl->peekedOffset < pImpl->peeked.size()) {
        count = std::min(capacity, pImpl->peeked.size() - pImpl->peekedOffset);
        std::copy_n(
            pImpl->peeked.data() + pImpl->peekedOffset, count, static_cast<uint8_t*>(buffer));
        pImpl->peekedOffset += count;
    } else {
        count = pImpl->readData(buffer, capacity);
    }
    pImpl->readProduced += count;
    if (pImpl->readProduced > info.size) {
        fail(ZipErrorCode::EntryTooLarge, "ZIP entry exceeds its declared size.", info.name);
    }
    if (count == 0 && pImpl->readProduced < info.size) {
        fail(ZipErrorCode::EntryTruncated, "Truncated ZIP entry.", info.name);
    }
    return count;
}
//=============================================================================
void
ZipArchive::closeEntry()
{
    pImpl->requireOpen(false);
    pImpl->entryOpen = false;
}
//=============================================================================
ZipArchive
ZipArchive::reopenForReading() const
{
    pImpl->requireOpen(false);
    auto impl = std::make_unique<Impl>();
    impl->sourceFile = pImpl->sourceFile;
    impl->memoryInput = pImpl->memoryInput;
    impl->entries = pImpl->entries;
    impl->index = pImpl->index;
    impl->directory = pImpl->directory;
    impl->ranks = pImpl->ranks;
    impl->password = pImpl->password;
    impl->open = true;
    return ZipArchive(std::move(impl));
}
//=============================================================================
void
ZipArchive::setPassword(const std::string& password)
{
    pImpl->password = password;
    pImpl->closeReader();
}
//=============================================================================
void
ZipArchive::setEncryption(ZipEncryption encryption, const std::string& password)
{
    if (encryption != ZipEncryption::None && password.empty()) {
        fail(ZipErrorCode::InvalidArgument, "Encryption needs a password.");
    }
    if (encryption == ZipEncryption::Aes192) {
        fail(ZipErrorCode::InvalidArgument,
            "AES-192 encryption cannot be written; use 'aes128' or 'aes256'.");
    }
    if (pImpl->writer != nullptr) {
        fail(ZipErrorCode::InvalidState, "Encryption must be chosen before the first entry.");
    }
    pImpl->encryption = encryption;
    pImpl->writePassword = password;
}
//=============================================================================
bool
ZipArchive::encryptionFromName(const std::string& name, ZipEncryption& encryption)
{
    std::string lower = name;
    std::transform(lower.begin(), lower.end(), lower.begin(),
        [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
    static const std::pair<const char*, ZipEncryption> names[] = { { "none", ZipEncryption::None },
        { "zipcrypto", ZipEncryption::ZipCrypto }, { "aes128", ZipEncryption::Aes128 },
        { "aes192", ZipEncryption::Aes192 }, { "aes256", ZipEncryption::Aes256 } };
    for (const auto& entry : names) {
        if (lower == entry.first) {
            encryption = entry.second;
            return true;
        }
    }
    return false;
}
//=============================================================================
void
ZipArchive::requireNoEntryBeingWritten() const
{
    if (pImpl && pImpl->open && pImpl->writing && pImpl->entryOpen) {
        fail(ZipErrorCode::EntryStillOpen, "A ZIP entry is still open for writing.",
            pImpl->currentEntry.name);
    }
}
//=============================================================================
std::vector<uint8_t>
ZipArchive::readEntry(size_t index, uint64_t maxSize)
{
    std::vector<uint8_t> result;
    if (index < pImpl->entries.size()) {
        result.reserve(ArchiveStreams::initialReservation(pImpl->entries[index].size, maxSize));
    }
    readEntry(
        index,
        [&result](
            const uint8_t* data, size_t size) { result.insert(result.end(), data, data + size); },
        maxSize);
    return result;
}
//=============================================================================
void
ZipArchive::beginEntry(const std::string& name, const ZipWriteOptions& options)
{
    pImpl->requireOpen(true);
    if (pImpl->entryOpen) {
        fail(ZipErrorCode::EntryStillOpen, "A ZIP entry is still open for writing.",
            pImpl->currentEntry.name);
    }
    if (name.empty()) {
        fail(ZipErrorCode::InvalidArgument, "ZIP entry name is empty.");
    }
    auto method = static_cast<uint16_t>(options.method);
    if (!supportsCompression(method)) {
        fail(ZipErrorCode::UnsupportedCompression, "Unsupported ZIP compression method.", name);
    }
    const bool isFolder = name.back() == '/';
    int level = options.level;
    // Level 0 stores the data, as zip tools do.
    if (isFolder || method == static_cast<uint16_t>(ZipCompression::Store) || level == 0) {
        method = static_cast<uint16_t>(ZipCompression::Store);
        level = defaultLevel;
    }
    ZipEntryInfo& entry = pImpl->currentEntry;
    entry = ZipEntryInfo();
    entry.name = name;
    entry.method = method;
    entry.versionMadeBy = unixMadeBy;
    entry.externalAttributes = options.externalAttributes;
    entry.modified = options.modified != 0 ? options.modified : time(nullptr);
    entry.isDirectory = isFolder;
    entry.isEncrypted = pImpl->encryption != ZipEncryption::None && !isFolder;
    pImpl->currentIsPending = isFolder && pImpl->writer == nullptr;
    if (!pImpl->currentIsPending) {
        if (isFolder) {
            pImpl->useLevel(defaultLevel);
        } else {
            pImpl->useLevel(level);
        }
        pImpl->writeHeader(entry, isFolder ? 0 : options.sizeHint);
    }
    pImpl->entryOpen = true;
}
//=============================================================================
void
ZipArchive::writeEntryData(const void* data, size_t size)
{
    pImpl->requireOpen(true);
    if (!pImpl->entryOpen) {
        fail(ZipErrorCode::InvalidState, "No ZIP entry is open.");
    }
    if (size == 0) {
        return;
    }
    if (pImpl->currentEntry.isDirectory) {
        fail(ZipErrorCode::InvalidArgument, "A ZIP folder has no data.", pImpl->currentEntry.name);
    }
    pImpl->writeData(data, size, pImpl->currentEntry.name);
    pImpl->currentEntry.size += size;
}
//=============================================================================
void
ZipArchive::endEntry()
{
    pImpl->requireOpen(true);
    if (!pImpl->entryOpen) {
        fail(ZipErrorCode::InvalidState, "No ZIP entry is open.");
    }
    pImpl->entryOpen = false;
    if (pImpl->currentIsPending) {
        pImpl->pendingFolders.push_back(pImpl->currentEntry);
    } else {
        pImpl->finishEntry(pImpl->currentEntry.name);
    }
    pImpl->record(pImpl->currentEntry);
}
//=============================================================================
void
ZipArchive::addBuffer(
    const std::string& name, const void* data, size_t size, const ZipWriteOptions& options)
{
    ZipWriteOptions sized = options;
    sized.sizeHint = static_cast<int64_t>(size);
    beginEntry(name, sized);
    writeEntryData(data, size);
    endEntry();
}
//=============================================================================
void
ZipArchive::addDirectory(const std::string& name, const ZipWriteOptions& options)
{
    ZipWriteOptions directory = options;
    directory.method = ZipCompression::Store;
    directory.level = 0;
    directory.sizeHint = 0;
    beginEntry(name.empty() || name.back() == '/' ? name : name + "/", directory);
    endEntry();
}
//=============================================================================
void
ZipArchive::addFile(
    const std::string& name, const std::wstring& filename, const ZipWriteOptions& options)
{
    std::ifstream input(toNativePath(filename), std::ios::in | std::ios::binary);
    if (!input.is_open()) {
        fail(ZipErrorCode::OpenFailed, "Cannot read file.", name);
    }
    ZipWriteOptions sized = options;
    std::error_code ec;
    const auto fileSize = std::filesystem::file_size(toNativePath(filename), ec);
    sized.sizeHint = ec ? -1 : static_cast<int64_t>(fileSize);
    beginEntry(name, sized);
    std::vector<char> block(blockSize);
    while (input) {
        ArchiveInterruption::throwIfInterrupted();
        input.read(block.data(), static_cast<std::streamsize>(block.size()));
        const auto count = static_cast<size_t>(input.gcount());
        if (count > 0) {
            writeEntryData(block.data(), count);
        }
    }
    if (input.bad()) {
        fail(ZipErrorCode::ReadFailed, "Cannot read file.", name);
    }
    endEntry();
}
//=============================================================================
void
ZipArchive::addFiles(const std::vector<ZipFileItem>& items)
{
    pImpl->requireOpen(true);
    for (const ZipFileItem& item : items) {
        ArchiveInterruption::throwIfInterrupted();
        if (item.isDirectory) {
            addDirectory(item.name, item.options);
        } else {
            addFile(item.name, item.filename, item.options);
        }
    }
}
//=============================================================================
void
ZipArchive::copyEntry(ZipArchive& source, size_t index, const std::string& name)
{
    pImpl->requireOpen(true);
    source.pImpl->requireOpen(false);
    if (index >= source.pImpl->entries.size()) {
        fail(ZipErrorCode::EntryNotFound, "ZIP entry index out of range.");
    }
    const ZipEntryInfo& entry = source.pImpl->entries[index];
    if (entry.isEncrypted) {
        fail(ZipErrorCode::InvalidArgument,
            "Encrypted ZIP entries cannot be copied: " + entry.name + ".", entry.name);
    }
    ZipWriteOptions options;
    options.method = isWritableMethod(entry.method) ? static_cast<ZipCompression>(entry.method)
                                                    : ZipCompression::Deflate;
    options.modified = entry.modified;
    options.externalAttributes = entry.externalAttributes;
    options.sizeHint = static_cast<int64_t>(entry.size);
    beginEntry(name, options);
    if (!entry.isDirectory) {
        source.readEntry(
            index, [&](const uint8_t* data, size_t size) { writeEntryData(data, size); });
    }
    endEntry();
}
//=============================================================================
void
ZipArchive::fileMetadata(
    const std::wstring& filename, time_t& modified, uint32_t& externalAttributes)
{
    // Unix mode in the high word, as zip tools store it.
    modified = 0;
    externalAttributes = 0;
#ifdef _WIN32
    struct _stat64 info;
    if (_wstat64(filename.c_str(), &info) != 0) {
        return;
    }
    const bool isFolder = (info.st_mode & _S_IFDIR) != 0;
    uint32_t mode = isFolder ? 040755 : 0100644;
    if (!isFolder && (info.st_mode & _S_IWRITE) == 0) {
        mode &= ~0222U;
    }
#else
    struct stat info;
    if (stat(wideToUtf8(filename).c_str(), &info) != 0) {
        return;
    }
    const auto mode = static_cast<uint32_t>(info.st_mode) & 0xFFFF;
#endif
    modified = static_cast<time_t>(info.st_mtime);
    externalAttributes = mode << 16;
}
//=============================================================================
void
ZipArchive::close()
{
    if (!pImpl || !pImpl->open) {
        return;
    }
    Impl& impl = *pImpl;
    if (!impl.writing) {
        impl.closeReader();
        impl.entryOpen = false;
        impl.open = false;
        return;
    }
    try {
        if (impl.entryOpen) {
            endEntry();
        }
        if (impl.writer == nullptr) {
            impl.startWriter(defaultLevel);
        }
        const int status = archive_write_close(impl.writer);
        archive_write_free(impl.writer);
        impl.writer = nullptr;
        if (status != ARCHIVE_OK) {
            fail(ZipErrorCode::WriteFailed, "Cannot write ZIP central directory.");
        }
        if (impl.replaceOnClose) {
            std::error_code ec;
            std::filesystem::rename(
                toNativePath(impl.writeTarget), toNativePath(impl.destination), ec);
            if (ec) {
                fail(ZipErrorCode::WriteFailed, "Cannot write ZIP archive.");
            }
            impl.replaceOnClose = false;
        }
        // Sizes, CRC and compressed sizes as libarchive wrote them.
        impl.setIndex(impl.destination.empty() ? indexMemory(impl.memoryOutput)
                                               : indexFile(impl.destination));
    } catch (...) {
        impl.open = false;
        if (impl.replaceOnClose) {
            std::error_code ec;
            std::filesystem::remove(toNativePath(impl.writeTarget), ec);
            impl.replaceOnClose = false;
        }
        throw;
    }
    impl.open = false;
}
//=============================================================================
bool
ZipArchive::isOpen() const
{
    return pImpl && pImpl->open;
}
//=============================================================================
const std::vector<uint8_t>&
ZipArchive::memoryBytes() const
{
    return pImpl->memoryOutput;
}
//=============================================================================
static void
fail(ZipErrorCode code, const std::string& message, const std::string& entry)
{
    throw ZipArchiveError(code, message, entry);
}
//=============================================================================
static std::vector<ZipDirectoryEntry>
indexFile(const std::wstring& filename)
{
    const auto path = toNativePath(filename);
    std::ifstream input(path, std::ios::binary);
    std::error_code ec;
    const auto size = std::filesystem::file_size(path, ec);
    if (!input.is_open() || ec) {
        fail(ZipErrorCode::OpenFailed, "Cannot open file.");
    }
    return readZipCentralDirectory(
        [&input](uint64_t offset, uint8_t* buffer, size_t count) {
            input.clear();
            input.seekg(static_cast<std::streamoff>(offset));
            input.read(reinterpret_cast<char*>(buffer), static_cast<std::streamsize>(count));
            return static_cast<size_t>(input.gcount());
        },
        static_cast<uint64_t>(size));
}
//=============================================================================
static std::vector<ZipDirectoryEntry>
indexMemory(const std::vector<uint8_t>& bytes)
{
    return readZipCentralDirectory(
        [&bytes](uint64_t offset, uint8_t* buffer, size_t count) {
            if (offset >= bytes.size()) {
                return size_t(0);
            }
            const size_t available = std::min<size_t>(count, bytes.size() - offset);
            std::copy_n(bytes.data() + offset, available, buffer);
            return available;
        },
        bytes.size());
}
//=============================================================================
static int
setWriteCompression(struct archive* archive, ZipCompression method)
{
    switch (method) {
    case ZipCompression::Store:
        return archive_write_zip_set_compression_store(archive);
    case ZipCompression::Deflate:
        return archive_write_zip_set_compression_deflate(archive);
#if ARCHIVE_VERSION_NUMBER >= 3008000
    case ZipCompression::Bzip2:
        return archive_write_zip_set_compression_bzip2(archive);
    case ZipCompression::Lzma:
        return archive_write_zip_set_compression_lzma(archive);
    case ZipCompression::Zstd:
        return archive_write_zip_set_compression_zstd(archive);
    case ZipCompression::Xz:
        return archive_write_zip_set_compression_xz(archive);
#endif
    default:
        return ARCHIVE_FAILED;
    }
}
//=============================================================================
static bool
isWritableMethod(uint16_t method)
{
    const auto methods = ZipArchive::availableCompressions();
    return std::find(methods.begin(), methods.end(), static_cast<ZipCompression>(method))
        != methods.end();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
