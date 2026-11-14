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
#if _MSC_VER
#pragma warning(disable : 4251)
#endif
//=============================================================================
#include <cstdint>
#include <ctime>
#include <functional>
#include <memory>
#include <optional>
#include <stdexcept>
#include <string>
#include <vector>
#include "nlsFile_archiver_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
// ZIP compression methods (values are the ZIP format codes).
enum class ZipCompression : uint16_t
{
    Store = 0,
    Deflate = 8,
    Bzip2 = 12,
    Lzma = 14,
    Zstd = 93,
    Xz = 95
};
//=============================================================================
enum class ZipErrorCode
{
    OpenFailed,
    NotAZipArchive,
    EntryNotFound,
    UnsupportedCompression,
    EncryptedEntry,
    WrongPassword,
    ReadFailed,
    CrcMismatch,
    EntryTooLarge,
    EntryTruncated,
    SizeLimitExceeded,
    EntryStillOpen,
    WriteFailed,
    InvalidState,
    InvalidArgument,
    DuplicateEntry,
    OverlappingEntries, // entries sharing archive bytes (zip bomb)
    UnsupportedEncryption // encrypted data the library cannot decrypt (7z, rar)
};
//=============================================================================
class NLSFILE_ARCHIVER_IMPEXP ZipArchiveError : public std::runtime_error
{
public:
    ZipArchiveError(ZipErrorCode code, const std::string& message, const std::string& entry = "")
        : std::runtime_error(message), errorCode(code), entryName(entry)
    {
    }
    ZipErrorCode
    code() const
    {
        return errorCode;
    }
    // UTF-8 name of the entry involved, empty when the error is about the archive.
    const std::string&
    entry() const
    {
        return entryName;
    }

private:
    ZipErrorCode errorCode;
    std::string entryName;
};
//=============================================================================
// Encryption of the entries written after ZipArchive::setEncryption.
enum class ZipEncryption
{
    None,
    ZipCrypto, // traditional PKWARE encryption (weak, for old readers)
    Aes128,
    Aes192,
    Aes256 // WinZip AES
};
//=============================================================================
struct ZipEntryInfo
{
    std::string name; // UTF-8 (CP437 names without the UTF-8 flag are converted)
    uint64_t size = 0;
    uint64_t compressedSize = 0;
    uint32_t crc32 = 0;
    uint16_t method = 0;
    uint16_t flags = 0;
    uint16_t versionMadeBy = 0;
    uint32_t externalAttributes = 0;
    time_t modified = 0;
    bool isDirectory = false;
    bool isSymlink = false;
    bool isEncrypted = false;
};
//=============================================================================
struct ZipWriteOptions
{
    ZipCompression method = ZipCompression::Deflate;
    int level = -1; // -1: method default
    time_t modified = 0; // 0: current time
    uint32_t externalAttributes = 0;
    int64_t sizeHint = -1; // known uncompressed size, -1 when unknown (selects ZIP64 early)
};
//=============================================================================
// File or folder to add with ZipArchive::addFiles.
struct ZipFileItem
{
    std::string name; // UTF-8 entry name
    std::wstring filename;
    ZipWriteOptions options;
    bool isDirectory = false;
};
//=============================================================================
// One ZIP archive on disk or in memory, read or written through libarchive.
class NLSFILE_ARCHIVER_IMPEXP ZipArchive
{
public:
    enum class Mode
    {
        Read,
        Create, // write a new archive, the file must not exist
        Overwrite, // write a new archive, replacing an existing file
        Append // add entries to an existing archive
    };

    static ZipArchive
    openFile(const std::wstring& filename, Mode mode);
    static ZipArchive
    openMemory(std::vector<uint8_t> bytes);
    static ZipArchive
    createInMemory();

    ZipArchive(ZipArchive&& other) noexcept;
    ZipArchive&
    operator=(ZipArchive&& other) noexcept;
    ZipArchive(const ZipArchive&) = delete;
    ZipArchive&
    operator=(const ZipArchive&)
        = delete;
    ~ZipArchive();

    static bool
    supportsCompression(uint16_t method);
    // "store", "deflate", "zstd", or "method N" for other codes.
    static std::string
    compressionName(uint16_t method);
    // Code of "store", "deflate" or "zstd" (case-insensitive); false for other names.
    static bool
    compressionFromName(const std::string& name, ZipCompression& method);
    // Methods this build can read and write.
    static std::vector<ZipCompression>
    availableCompressions();

    // UTF-8 password used to read encrypted entries (WinZip AES or traditional PKWARE).
    void
    setPassword(const std::string& password);
    // Encrypts the files written from now on with password (folders stay clear).
    void
    setEncryption(ZipEncryption encryption, const std::string& password);
    // "none", "zipcrypto", "aes128", "aes192" or "aes256" (case-insensitive).
    static bool
    encryptionFromName(const std::string& name, ZipEncryption& encryption);

    // Reading: the central directory is indexed once at opening.
    const std::vector<ZipEntryInfo>&
    entries() const;
    // Index of the last entry with this exact name.
    std::optional<size_t>
    find(const std::string& name) const;
    // Streams the entry through sink in blocks, checks size and CRC at the end.
    // Entries whose declared size exceeds maxSize are refused before decompression.
    void
    readEntry(size_t index, const std::function<void(const uint8_t*, size_t)>& sink,
        uint64_t maxSize = UINT64_MAX);
    std::vector<uint8_t>
    readEntry(size_t index, uint64_t maxSize = UINT64_MAX);
    // Pull reading of one entry: open, read blocks (0 at the end), close checks the CRC.
    void
    openEntry(size_t index, uint64_t maxSize = UINT64_MAX);
    size_t
    readEntryData(void* buffer, size_t capacity);
    void
    closeEntry();
    // Independent read handle on the same file or bytes (concurrent entry readers).
    ZipArchive
    reopenForReading() const;
    // Throws EntryStillOpen between beginEntry and endEntry.
    void
    requireNoEntryBeingWritten() const;

    // Writing.
    void
    beginEntry(const std::string& name, const ZipWriteOptions& options);
    void
    writeEntryData(const void* data, size_t size);
    void
    endEntry();
    void
    addBuffer(
        const std::string& name, const void* data, size_t size, const ZipWriteOptions& options);
    void
    addDirectory(const std::string& name, const ZipWriteOptions& options);
    void
    addFile(const std::string& name, const std::wstring& filename, const ZipWriteOptions& options);
    // Adds items in order.
    void
    addFiles(const std::vector<ZipFileItem>& items);
    // Copies an entry of source under a new name (decompressed and compressed again).
    void
    copyEntry(ZipArchive& source, size_t index, const std::string& name);
    // Modification time and attributes of a file, as zip stores them.
    static void
    fileMetadata(const std::wstring& filename, time_t& modified, uint32_t& externalAttributes);

    // Writes the central directory (write modes) and releases the archive.
    void
    close();
    bool
    isOpen() const;
    // Archive bytes of an in-memory archive after close().
    const std::vector<uint8_t>&
    memoryBytes() const;

private:
    struct Impl;
    explicit ZipArchive(std::unique_ptr<Impl> impl);
    std::unique_ptr<Impl> pImpl;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
