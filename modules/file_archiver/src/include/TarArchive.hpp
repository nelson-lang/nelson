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
#include <ctime>
#include <memory>
#include <optional>
#include <string>
#include <vector>
#include "ZipArchive.hpp"
#include "nlsFile_archiver_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
// Compression of a tar stream (Other: a reader found a compression without a name here).
enum class TarCompression
{
    None,
    Gzip,
    Zstd,
    Xz,
    Bzip2,
    Lz4,
    Lzma,
    Other
};
//=============================================================================
struct TarEntryInfo
{
    std::string name; // UTF-8, folders end with '/'
    uint64_t size = 0;
    time_t modified = 0;
    uint32_t mode = 0;
    bool isDirectory = false;
    bool isSymlink = false; // symbolic or hard link
    bool isHardlink = false;
    bool isEncrypted = false;
    std::string linkName;
    uint64_t headerIndex = 0; // rank of the entry header in the archive
};
//=============================================================================
// Reading options: any format libarchive reads (7z, rar, zip, iso, cpio...) instead of tar
// only, and the password of encrypted entries.
struct TarReadOptions
{
    bool anyFormat = false;
    std::string password;
};
//=============================================================================
// tar archive (ustar with pax names) on disk or in memory, read and written by libarchive.
// Errors are ZipArchiveError values with the same codes as ZIP archives.
class NLSFILE_ARCHIVER_IMPEXP TarArchive
{
public:
    // Reading: compression and format are found from the data, entries are indexed once.
    static TarArchive
    openFile(const std::wstring& filename, const TarReadOptions& options = TarReadOptions());
    static TarArchive
    openMemory(std::vector<uint8_t> bytes, const TarReadOptions& options = TarReadOptions());
    // Writing: a file is written under a temporary name and renamed by close().
    // threads: zstd and xz compression threads (0: all cores).
    static TarArchive
    createFile(
        const std::wstring& filename, TarCompression compression, int level, int threads = 0);
    static TarArchive
    createInMemory(TarCompression compression, int level, int threads = 0);

    TarArchive(TarArchive&& other) noexcept;
    TarArchive&
    operator=(TarArchive&& other) noexcept;
    TarArchive(const TarArchive&) = delete;
    TarArchive&
    operator=(const TarArchive&) = delete;
    ~TarArchive();

    // "none", "gzip", "zstd", "xz", "bzip2", "lz4", "lzma" or "other".
    static std::string
    compressionName(TarCompression compression);
    // Name of a compression writers use (case-insensitive); false for other names.
    static bool
    compressionFromName(const std::string& name, TarCompression& compression);
    // Compression suggested by the file name (.tar.gz, .tgz, .tar.zst, .tar.xz...), else none.
    static TarCompression
    compressionFromFilename(const std::wstring& filename);
    // Compressions this build writes and reads.
    static std::vector<TarCompression>
    availableCompressions();
    // Levels accepted for a compression; false when it takes no level.
    static bool
    levelRange(TarCompression compression, int& minimum, int& maximum);

    TarCompression
    compression() const;
    // Format found by a reader ("POSIX ustar format", "7-Zip", "RAR5"...).
    std::string
    formatName() const;
    bool
    isWriting() const;
    // Archive read: file name (empty for bytes) and compressed size in bytes.
    std::wstring
    sourceName() const;
    uint64_t
    sourceSize() const;
    const std::vector<TarEntryInfo>&
    entries() const;
    // Index of the last entry with this exact name (folders without their '/').
    std::optional<size_t>
    find(const std::string& name) const;
    std::vector<uint8_t>
    readEntry(size_t index, uint64_t maxSize = UINT64_MAX);
    // Pull reading of one entry: open, read blocks (0 at the end).
    void
    openEntry(size_t index, uint64_t maxSize = UINT64_MAX);
    size_t
    readEntryData(void* buffer, size_t capacity);
    // Independent read handle on the same file or bytes.
    TarArchive
    reopenForReading() const;

    void
    addBuffer(const std::string& name, const void* data, size_t size, time_t modified = 0,
        uint32_t mode = 0644);
    void
    addFile(const std::string& name, const std::wstring& filename);
    void
    addDirectory(const std::string& name, time_t modified = 0, uint32_t mode = 0755);
    // Adds files and folders in order (ZipFileItem options are not used).
    void
    addFiles(const std::vector<ZipFileItem>& items);

    // Ends a writer (end blocks, compression, rename) and releases the archive.
    void
    close();
    bool
    isOpen() const;
    // Archive bytes of an in-memory writer after close().
    const std::vector<uint8_t>&
    memoryBytes() const;

private:
    struct Impl;
    explicit TarArchive(std::unique_ptr<Impl> impl);
    std::unique_ptr<Impl> pImpl;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
