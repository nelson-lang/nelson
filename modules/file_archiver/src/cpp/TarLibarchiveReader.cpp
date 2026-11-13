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
#include <cerrno>
#include "LibarchiveSupport.hpp"
#include "TarLibarchiveReader.hpp"
#include "ArchivePath.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static constexpr size_t readBlock = 64 * 1024;
//=============================================================================
[[noreturn]] static void
fail(ZipErrorCode code, const std::string& message, const std::string& entry = "");
static void
supportFormats(struct archive* handle, bool anyFormat);
static TarCompression
compressionOfFilter(int code);
static bool
isFormatError(int code);
//=============================================================================
TarLibarchiveReader::TarLibarchiveReader(std::shared_ptr<const std::vector<uint8_t>> bytes,
    const std::wstring& file, const TarReadOptions& readOptions)
    : memory(std::move(bytes)), filename(file), options(readOptions)
{
    reopen();
}
//=============================================================================
TarLibarchiveReader::~TarLibarchiveReader() { close(); }
//=============================================================================
void
TarLibarchiveReader::close()
{
    if (handle != nullptr) {
        archive_read_free(handle);
        handle = nullptr;
    }
    headersRead = 0;
}
//=============================================================================
void
TarLibarchiveReader::reopen()
{
    close();
    handle = archive_read_new();
    if (handle == nullptr) {
        fail(ZipErrorCode::OpenFailed, "Cannot allocate tar reader.");
    }
    archive_read_support_filter_all(handle);
    supportFormats(handle, options.anyFormat);
    // tar names outside pax records are taken as UTF-8.
    archive_read_set_options(handle, "tar:hdrcharset=UTF-8");
    if (!options.password.empty()) {
        archive_read_add_passphrase(handle, options.password.c_str());
    }
    int status = ARCHIVE_FATAL;
    if (memory) {
        status = archive_read_open_memory(handle, memory->data(), memory->size());
    } else {
#ifdef _WIN32
        status = archive_read_open_filename_w(handle, filename.c_str(), readBlock);
#else
        status = archive_read_open_filename(handle, wideToUtf8(filename).c_str(), readBlock);
#endif
    }
    if (status != ARCHIVE_OK) {
        // libarchive recognizes the format while opening.
        const bool wrongFormat = isFormatError(archive_errno(handle));
        const std::string message = libarchiveErrorText(handle, "Cannot open archive.");
        close();
        if (wrongFormat) {
            fail(ZipErrorCode::NotAZipArchive,
                options.anyFormat ? "Unknown archive format." : "Not a tar archive.");
        }
        fail(ZipErrorCode::OpenFailed, message);
    }
}
//=============================================================================
void
TarLibarchiveReader::scan(
    std::vector<TarEntryInfo>& entries, TarCompression& compression, std::string& format)
{
    entries.clear();
    for (;;) {
        struct archive_entry* item = nullptr;
        const int status = archive_read_next_header(handle, &item);
        if (status == ARCHIVE_EOF) {
            break;
        }
        if (status < ARCHIVE_WARN || item == nullptr) {
            fail(headersRead == 0 ? ZipErrorCode::NotAZipArchive : ZipErrorCode::EntryTruncated,
                headersRead == 0 ? "Not a tar archive."
                                 : libarchiveErrorText(handle, "Truncated tar data."));
        }
        ++headersRead;
        const char* utf8 = archive_entry_pathname_utf8(item);
        const char* raw = archive_entry_pathname(item);
        TarEntryInfo entry;
        entry.name = utf8 != nullptr ? utf8 : (raw != nullptr ? raw : "");
        entry.modified = archive_entry_mtime(item);
        entry.mode = static_cast<uint32_t>(archive_entry_perm(item)) & 07777;
        entry.headerIndex = headersRead - 1;
        entry.isEncrypted = archive_entry_is_encrypted(item) != 0;
        if (headersRead == 1) {
            const char* name = archive_format_name(handle);
            format = name != nullptr ? name : "";
        }
        const char* hardlink = archive_entry_hardlink(item);
        const auto type = archive_entry_filetype(item);
        if (hardlink != nullptr || type == AE_IFLNK) {
            entry.isSymlink = true;
            entry.isHardlink = hardlink != nullptr;
            const char* target = hardlink != nullptr ? hardlink : archive_entry_symlink(item);
            entry.linkName = target != nullptr ? target : "";
        } else if (type == AE_IFDIR) {
            entry.isDirectory = true;
            if (entry.name.empty() || entry.name.back() != '/') {
                entry.name += "/";
            }
        } else if (type == AE_IFREG) {
            entry.size = static_cast<uint64_t>(std::max<la_int64_t>(archive_entry_size(item), 0));
        } else {
            // Devices, fifos and sockets are not extracted.
            continue;
        }
        entries.push_back(entry);
    }
    if (headersRead == 0 && archive_filter_bytes(handle, -1) == 0) {
        fail(ZipErrorCode::NotAZipArchive, "Not a tar archive.");
    }
    compression = compressionOfFilter(archive_filter_code(handle, 0));
}
//=============================================================================
void
TarLibarchiveReader::openEntry(const TarEntryInfo& entry)
{
    currentName = entry.name;
    if (headersRead > entry.headerIndex) {
        reopen();
    }
    while (headersRead <= entry.headerIndex) {
        struct archive_entry* item = nullptr;
        const int status = archive_read_next_header(handle, &item);
        if (status < ARCHIVE_WARN || status == ARCHIVE_EOF) {
            fail(ZipErrorCode::EntryTruncated,
                libarchiveErrorText(handle, "Truncated tar data."), entry.name);
        }
        ++headersRead;
    }
}
//=============================================================================
size_t
TarLibarchiveReader::read(uint8_t* buffer, size_t size)
{
    size_t got = 0;
    while (got < size) {
        const la_ssize_t count = archive_read_data(handle, buffer + got, size - got);
        if (count < 0) {
            fail(libarchiveReadErrorCode(handle),
                libarchiveErrorText(handle, "Cannot read archive data."), currentName);
        }
        if (count == 0) {
            break;
        }
        got += static_cast<size_t>(count);
    }
    return got;
}
//=============================================================================
static void
fail(ZipErrorCode code, const std::string& message, const std::string& entry)
{
    throw ZipArchiveError(code, message, entry);
}
//=============================================================================
static void
supportFormats(struct archive* handle, bool anyFormat)
{
    archive_read_support_format_tar(handle);
    if (!anyFormat) {
        return;
    }
    // Formats read through a seekable source (zip by its central directory).
    archive_read_support_format_7zip(handle);
    archive_read_support_format_ar(handle);
    archive_read_support_format_cab(handle);
    archive_read_support_format_cpio(handle);
    archive_read_support_format_iso9660(handle);
    archive_read_support_format_lha(handle);
    archive_read_support_format_rar(handle);
    archive_read_support_format_rar5(handle);
    archive_read_support_format_xar(handle);
    archive_read_support_format_zip_seekable(handle);
}
//=============================================================================
static TarCompression
compressionOfFilter(int code)
{
    switch (code) {
    case ARCHIVE_FILTER_NONE:
        return TarCompression::None;
    case ARCHIVE_FILTER_GZIP:
        return TarCompression::Gzip;
    case ARCHIVE_FILTER_ZSTD:
        return TarCompression::Zstd;
    case ARCHIVE_FILTER_XZ:
        return TarCompression::Xz;
    case ARCHIVE_FILTER_BZIP2:
        return TarCompression::Bzip2;
    case ARCHIVE_FILTER_LZ4:
        return TarCompression::Lz4;
    case ARCHIVE_FILTER_LZMA:
        return TarCompression::Lzma;
    default:
        return TarCompression::Other;
    }
}
//=============================================================================
static bool
isFormatError(int code)
{
    // libarchive reports an unknown format with EFTYPE where it exists, else EILSEQ.
#ifdef EFTYPE
    return code == EFTYPE;
#else
    return code == EILSEQ;
#endif
}
//=============================================================================
} // namespace Nelson
//=============================================================================
