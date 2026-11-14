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
#include <cwctype>
#include <filesystem>
#include <fstream>
#include <thread>
#include <unordered_map>
#include "LibarchiveSupport.hpp"
#include "TarArchive.hpp"
#include "TarLibarchiveReader.hpp"
#include "ArchiveInterruption.hpp"
#include "ArchivePath.hpp"
#include "ArchiveStreams.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    constexpr size_t copyBlock = 64 * 1024;
} // namespace
//=============================================================================
[[noreturn]] static void
fail(ZipErrorCode code, const std::string& message, const std::string& entry = "");
static std::string
withoutTrailingSlash(const std::string& name);
static int
addFilter(struct archive* writer, TarCompression compression);
//=============================================================================
struct TarArchive::Impl
{
    bool writing = false;
    bool open = false;
    TarCompression compression = TarCompression::None;
    std::vector<TarEntryInfo> entries;
    std::unordered_map<std::string, size_t> index;
    // Reading.
    std::wstring sourceFile;
    std::shared_ptr<const std::vector<uint8_t>> memory;
    TarReadOptions options;
    std::unique_ptr<TarLibarchiveReader> reader;
    std::string format;
    uint64_t remaining = 0;
    // Writing.
    struct archive* writer = nullptr;
    std::wstring destination;
    std::wstring temporary;
    std::vector<uint8_t> memoryOutput;

    ~Impl()
    {
        if (writer != nullptr) {
            archive_write_free(writer);
            writer = nullptr;
        }
        if (writing && open && !temporary.empty()) {
            std::error_code ec;
            std::filesystem::remove(toNativePath(temporary), ec);
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
    initReading()
    {
        reader = std::make_unique<TarLibarchiveReader>(memory, sourceFile, options);
        writing = false;
        open = true;
    }

    // Indexes every entry once.
    void
    scan()
    {
        reader->scan(entries, compression, format);
        for (size_t k = 0; k < entries.size(); ++k) {
            index[withoutTrailingSlash(entries[k].name)] = k;
        }
    }

    void
    startWriter(int level, int threads)
    {
        writer = archive_write_new();
        if (writer == nullptr) {
            fail(ZipErrorCode::OpenFailed, "Cannot allocate tar writer.");
        }
        // ustar, with pax records for long or non-ASCII names and big sizes.
        archive_write_set_format_pax_restricted(writer);
        // Entry names are UTF-8 bytes set as plain pathnames. hdrcharset=BINARY
        // makes the pax writer emit them verbatim instead of converting through
        // the C locale, which yields a NULL path (crash) or mojibake.
        archive_write_set_options(writer, "hdrcharset=BINARY");
        if (addFilter(writer, compression) != ARCHIVE_OK) {
            fail(ZipErrorCode::UnsupportedCompression,
                "This compression is not available in this build.");
        }
        if (level >= 0) {
            const std::string value = std::to_string(level);
            if (archive_write_set_filter_option(writer, nullptr, "compression-level", value.c_str())
                != ARCHIVE_OK) {
                fail(ZipErrorCode::InvalidArgument, "Invalid compression level.");
            }
        }
        if (compression == TarCompression::Gzip) {
            // No date in the gzip header: the same files give the same archive.
            archive_write_set_filter_option(writer, "gzip", "timestamp", nullptr);
        }
        if (compression == TarCompression::Zstd || compression == TarCompression::Xz) {
            const unsigned cores = std::max(1U, std::thread::hardware_concurrency());
            const std::string value = std::to_string(threads > 0 ? threads : cores);
            archive_write_set_filter_option(writer, nullptr, "threads", value.c_str());
        }
        // No zero padding after the end of the archive (a zstd reader refuses it).
        archive_write_set_bytes_in_last_block(writer, 1);
        int status = ARCHIVE_FATAL;
        if (temporary.empty()) {
            status = archive_write_open2(writer, this, nullptr, appendToMemory, nullptr, nullptr);
        } else {
#ifdef _WIN32
            status = archive_write_open_filename_w(writer, temporary.c_str());
#else
            status = archive_write_open_filename(writer, wideToUtf8(temporary).c_str());
#endif
        }
        if (status != ARCHIVE_OK) {
            fail(ZipErrorCode::OpenFailed, "Cannot create tar archive.");
        }
    }

    void
    writeHeader(
        const std::string& name, uint64_t size, time_t modified, uint32_t mode, bool isDirectory)
    {
        if (!writing || !open) {
            fail(ZipErrorCode::InvalidState, "The tar archive is not open for writing.");
        }
        if (name.empty()) {
            fail(ZipErrorCode::InvalidArgument, "tar entry name is empty.");
        }
        struct archive_entry* item = archive_entry_new();
        archive_entry_set_pathname(item, name.c_str());
        archive_entry_set_filetype(item, isDirectory ? AE_IFDIR : AE_IFREG);
        archive_entry_set_perm(item, mode & 07777);
        archive_entry_set_size(item, static_cast<la_int64_t>(size));
        archive_entry_set_mtime(item, modified != 0 ? modified : time(nullptr), 0);
        const int status = archive_write_header(writer, item);
        archive_entry_free(item);
        if (status != ARCHIVE_OK) {
            fail(ZipErrorCode::WriteFailed, libarchiveErrorText(writer, "Cannot write tar entry."),
                name);
        }
    }

    void
    writeData(const void* data, size_t size, const std::string& name)
    {
        if (size > 0 && archive_write_data(writer, data, size) != static_cast<la_ssize_t>(size)) {
            fail(ZipErrorCode::WriteFailed, libarchiveErrorText(writer, "Cannot write tar data."),
                name);
        }
    }

    // Lists a written entry, so that entries() and find() also work on writers.
    void
    record(const std::string& name, uint64_t size, time_t modified, uint32_t mode, bool isDirectory)
    {
        TarEntryInfo entry;
        entry.name = name;
        entry.size = size;
        entry.modified = modified != 0 ? modified : time(nullptr);
        entry.mode = mode;
        entry.isDirectory = isDirectory;
        index[withoutTrailingSlash(name)] = entries.size();
        entries.push_back(entry);
    }
};
//=============================================================================
TarArchive::TarArchive(std::unique_ptr<Impl> impl) : pImpl(std::move(impl)) { }
//=============================================================================
TarArchive::TarArchive(TarArchive&& other) noexcept = default;
//=============================================================================
TarArchive&
TarArchive::operator=(TarArchive&& other) noexcept
    = default;
//=============================================================================
TarArchive::~TarArchive() = default;
//=============================================================================
TarArchive
TarArchive::openFile(const std::wstring& filename, const TarReadOptions& options)
{
    auto impl = std::make_unique<Impl>();
    impl->sourceFile = filename;
    impl->options = options;
    impl->initReading();
    impl->scan();
    return TarArchive(std::move(impl));
}
//=============================================================================
TarArchive
TarArchive::openMemory(std::vector<uint8_t> bytes, const TarReadOptions& options)
{
    auto impl = std::make_unique<Impl>();
    impl->memory = std::make_shared<const std::vector<uint8_t>>(std::move(bytes));
    impl->options = options;
    impl->initReading();
    impl->scan();
    return TarArchive(std::move(impl));
}
//=============================================================================
TarArchive
TarArchive::createFile(
    const std::wstring& filename, TarCompression compression, int level, int threads)
{
    auto impl = std::make_unique<Impl>();
    impl->writing = true;
    impl->open = true;
    impl->compression = compression;
    impl->destination = filename;
    impl->temporary = filename + L".nelson-tmp";
    try {
        impl->startWriter(level, threads);
    } catch (...) {
        impl->temporary.clear();
        throw;
    }
    return TarArchive(std::move(impl));
}
//=============================================================================
TarArchive
TarArchive::createInMemory(TarCompression compression, int level, int threads)
{
    auto impl = std::make_unique<Impl>();
    impl->writing = true;
    impl->open = true;
    impl->compression = compression;
    impl->startWriter(level, threads);
    return TarArchive(std::move(impl));
}
//=============================================================================
std::string
TarArchive::compressionName(TarCompression compression)
{
    switch (compression) {
    case TarCompression::Gzip:
        return "gzip";
    case TarCompression::Zstd:
        return "zstd";
    case TarCompression::Xz:
        return "xz";
    case TarCompression::Bzip2:
        return "bzip2";
    case TarCompression::Lz4:
        return "lz4";
    case TarCompression::Lzma:
        return "lzma";
    case TarCompression::Other:
        return "other";
    case TarCompression::None:
    default:
        return "none";
    }
}
//=============================================================================
bool
TarArchive::compressionFromName(const std::string& name, TarCompression& compression)
{
    std::string lower = name;
    std::transform(lower.begin(), lower.end(), lower.begin(),
        [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
    for (TarCompression value :
        { TarCompression::None, TarCompression::Gzip, TarCompression::Zstd, TarCompression::Xz,
            TarCompression::Bzip2, TarCompression::Lz4, TarCompression::Lzma }) {
        if (lower == compressionName(value)) {
            compression = value;
            return true;
        }
    }
    return false;
}
//=============================================================================
TarCompression
TarArchive::compressionFromFilename(const std::wstring& filename)
{
    std::wstring lower = filename;
    std::transform(lower.begin(), lower.end(), lower.begin(),
        [](wchar_t c) { return static_cast<wchar_t>(std::towlower(c)); });
    auto endsWith = [&](const std::wstring& suffix) {
        return lower.size() >= suffix.size()
            && lower.compare(lower.size() - suffix.size(), suffix.size(), suffix) == 0;
    };
    static const std::pair<const wchar_t*, TarCompression> suffixes[]
        = { { L".tar.gz", TarCompression::Gzip }, { L".tgz", TarCompression::Gzip },
              { L".tar.zst", TarCompression::Zstd }, { L".tzst", TarCompression::Zstd },
              { L".tar.xz", TarCompression::Xz }, { L".txz", TarCompression::Xz },
              { L".tar.bz2", TarCompression::Bzip2 }, { L".tbz2", TarCompression::Bzip2 },
              { L".tbz", TarCompression::Bzip2 }, { L".tar.lz4", TarCompression::Lz4 },
              { L".tar.lzma", TarCompression::Lzma }, { L".tlz", TarCompression::Lzma } };
    for (const auto& [suffix, compression] : suffixes) {
        if (endsWith(suffix)) {
            return compression;
        }
    }
    return TarCompression::None;
}
//=============================================================================
std::vector<TarCompression>
TarArchive::availableCompressions()
{
    static const std::vector<TarCompression> compressions = []() {
        std::vector<TarCompression> found;
        for (TarCompression candidate :
            { TarCompression::None, TarCompression::Gzip, TarCompression::Zstd, TarCompression::Xz,
                TarCompression::Bzip2, TarCompression::Lz4, TarCompression::Lzma }) {
            struct archive* probe = archive_write_new();
            if (addFilter(probe, candidate) == ARCHIVE_OK) {
                found.push_back(candidate);
            }
            archive_write_free(probe);
        }
        return found;
    }();
    return compressions;
}
//=============================================================================
bool
TarArchive::levelRange(TarCompression compression, int& minimum, int& maximum)
{
    switch (compression) {
    case TarCompression::Gzip:
    case TarCompression::Xz:
    case TarCompression::Lzma:
        minimum = 0;
        maximum = 9;
        return true;
    case TarCompression::Bzip2:
    case TarCompression::Lz4:
        minimum = 1;
        maximum = 9;
        return true;
    case TarCompression::Zstd:
        minimum = 1;
        maximum = 22;
        return true;
    default:
        return false;
    }
}
//=============================================================================
TarCompression
TarArchive::compression() const
{
    return pImpl->compression;
}
//=============================================================================
std::string
TarArchive::formatName() const
{
    return pImpl->writing ? std::string("POSIX pax restricted") : pImpl->format;
}
//=============================================================================
bool
TarArchive::isWriting() const
{
    return pImpl->writing;
}
//=============================================================================
std::wstring
TarArchive::sourceName() const
{
    return pImpl->sourceFile;
}
//=============================================================================
uint64_t
TarArchive::sourceSize() const
{
    if (pImpl->memory) {
        return pImpl->memory->size();
    }
    std::error_code ec;
    const auto size = std::filesystem::file_size(toNativePath(pImpl->sourceFile), ec);
    return ec ? 0 : static_cast<uint64_t>(size);
}
//=============================================================================
const std::vector<TarEntryInfo>&
TarArchive::entries() const
{
    return pImpl->entries;
}
//=============================================================================
std::optional<size_t>
TarArchive::find(const std::string& name) const
{
    auto it = pImpl->index.find(withoutTrailingSlash(name));
    if (it == pImpl->index.end()) {
        return std::nullopt;
    }
    return it->second;
}
//=============================================================================
std::vector<uint8_t>
TarArchive::readEntry(size_t index, uint64_t maxSize)
{
    openEntry(index, maxSize);
    std::vector<uint8_t> data;
    data.reserve(ArchiveStreams::initialReservation(pImpl->remaining, maxSize));
    std::vector<uint8_t> block(copyBlock);
    while (pImpl->remaining > 0) {
        const size_t count = readEntryData(block.data(), block.size());
        data.insert(data.end(), block.data(), block.data() + count);
    }
    return data;
}
//=============================================================================
void
TarArchive::openEntry(size_t index, uint64_t maxSize)
{
    if (!pImpl->open || pImpl->writing) {
        fail(ZipErrorCode::InvalidState, "The tar archive is not open for reading.");
    }
    if (index >= pImpl->entries.size()) {
        fail(ZipErrorCode::EntryNotFound, "tar entry index out of range.");
    }
    const TarEntryInfo& entry = pImpl->entries[index];
    if (entry.size > maxSize) {
        fail(ZipErrorCode::SizeLimitExceeded, "tar entry exceeds the size limit.", entry.name);
    }
    if (entry.isEncrypted && pImpl->options.password.empty()) {
        fail(ZipErrorCode::EncryptedEntry, "Encrypted entry needs a password.", entry.name);
    }
    pImpl->reader->openEntry(entry);
    pImpl->remaining = entry.size;
}
//=============================================================================
size_t
TarArchive::readEntryData(void* buffer, size_t capacity)
{
    const size_t wanted = static_cast<size_t>(std::min<uint64_t>(capacity, pImpl->remaining));
    const size_t got = pImpl->reader->read(static_cast<uint8_t*>(buffer), wanted);
    if (got != wanted) {
        fail(ZipErrorCode::EntryTruncated, "Truncated tar data.");
    }
    pImpl->remaining -= got;
    return got;
}
//=============================================================================
TarArchive
TarArchive::reopenForReading() const
{
    auto impl = std::make_unique<Impl>();
    impl->sourceFile = pImpl->sourceFile;
    impl->memory = pImpl->memory;
    impl->options = pImpl->options;
    impl->format = pImpl->format;
    impl->compression = pImpl->compression;
    impl->initReading();
    impl->entries = pImpl->entries;
    impl->index = pImpl->index;
    return TarArchive(std::move(impl));
}
//=============================================================================
void
TarArchive::addBuffer(
    const std::string& name, const void* data, size_t size, time_t modified, uint32_t mode)
{
    pImpl->writeHeader(name, size, modified, mode, false);
    pImpl->writeData(data, size, name);
    pImpl->record(name, size, modified, mode, false);
}
//=============================================================================
void
TarArchive::addFile(const std::string& name, const std::wstring& filename)
{
    namespace fs = std::filesystem;
    const fs::path path = toNativePath(filename);
    std::error_code ec;
    const uint64_t size = fs::file_size(path, ec);
    std::ifstream input(path, std::ios::binary);
    if (ec || !input.is_open()) {
        fail(ZipErrorCode::OpenFailed, "Cannot read file.", name);
    }
    time_t modified = 0;
    uint32_t attributes = 0;
    ZipArchive::fileMetadata(filename, modified, attributes);
    const uint32_t permissions = (attributes >> 16) & 0777;
    const uint32_t mode = permissions == 0 ? 0644 : permissions;
    pImpl->writeHeader(name, size, modified, mode, false);
    std::vector<char> block(copyBlock);
    uint64_t copied = 0;
    while (copied < size) {
        ArchiveInterruption::throwIfInterrupted();
        input.read(block.data(),
            static_cast<std::streamsize>(std::min<uint64_t>(block.size(), size - copied)));
        const auto count = static_cast<size_t>(input.gcount());
        if (count == 0) {
            fail(ZipErrorCode::ReadFailed, "Cannot read file.", name);
        }
        pImpl->writeData(block.data(), count, name);
        copied += count;
    }
    pImpl->record(name, size, modified, mode, false);
}
//=============================================================================
void
TarArchive::addDirectory(const std::string& name, time_t modified, uint32_t mode)
{
    const std::string folder = withoutTrailingSlash(name) + "/";
    pImpl->writeHeader(folder, 0, modified, mode, true);
    pImpl->record(folder, 0, modified, mode, true);
}
//=============================================================================
void
TarArchive::addFiles(const std::vector<ZipFileItem>& items)
{
    for (const ZipFileItem& item : items) {
        ArchiveInterruption::throwIfInterrupted();
        if (item.isDirectory) {
            addDirectory(item.name, item.options.modified);
        } else {
            addFile(item.name, item.filename);
        }
    }
}
//=============================================================================
void
TarArchive::close()
{
    Impl& impl = *pImpl;
    if (!impl.open) {
        return;
    }
    if (!impl.writing) {
        impl.open = false;
        impl.reader.reset();
        return;
    }
    const int status = archive_write_close(impl.writer);
    archive_write_free(impl.writer);
    impl.writer = nullptr;
    impl.open = false;
    std::error_code ec;
    if (status != ARCHIVE_OK) {
        ec = std::make_error_code(std::errc::io_error);
    } else if (!impl.destination.empty()) {
        std::filesystem::rename(toNativePath(impl.temporary), toNativePath(impl.destination), ec);
    }
    if (ec) {
        if (!impl.temporary.empty()) {
            std::filesystem::remove(toNativePath(impl.temporary), ec);
        }
        impl.temporary.clear();
        fail(ZipErrorCode::WriteFailed, "Cannot write tar archive.");
    }
    impl.temporary.clear();
}
//=============================================================================
bool
TarArchive::isOpen() const
{
    return pImpl && pImpl->open;
}
//=============================================================================
const std::vector<uint8_t>&
TarArchive::memoryBytes() const
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
static std::string
withoutTrailingSlash(const std::string& name)
{
    return !name.empty() && name.back() == '/' ? name.substr(0, name.size() - 1) : name;
}
//=============================================================================
static int
addFilter(struct archive* writer, TarCompression compression)
{
    switch (compression) {
    case TarCompression::None:
        return archive_write_add_filter_none(writer);
    case TarCompression::Gzip:
        return archive_write_add_filter_gzip(writer);
    case TarCompression::Zstd:
        return archive_write_add_filter_zstd(writer);
    case TarCompression::Xz:
        return archive_write_add_filter_xz(writer);
    case TarCompression::Bzip2:
        return archive_write_add_filter_bzip2(writer);
    case TarCompression::Lz4:
        return archive_write_add_filter_lz4(writer);
    case TarCompression::Lzma:
        return archive_write_add_filter_lzma(writer);
    default:
        return ARCHIVE_FATAL;
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
