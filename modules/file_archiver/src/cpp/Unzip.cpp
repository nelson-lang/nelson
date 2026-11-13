//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif
#include <cstdio>
#include <filesystem>
#include <optional>
#include "FileSystemWrapper.hpp"
#include "Unzip.hpp"
#include "ArchiveDiskWriter.hpp"
#include "ArchiveInterruption.hpp"
#include "ZipArchive.hpp"
#include "ZipHelpers.hpp"
#include "characters_encoding.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Extracts one file entry; reports read errors and write errors separately.
static void
extractFileEntry(ZipArchive& archive, size_t index, ArchiveDiskWriter& disk,
    const std::wstring& onDisk, const std::wstring& destination);
// Mode written on disk: execute bits of Unix entries only, the umask applies.
static uint32_t
diskMode(const ZipEntryInfo& entry);
// Refuses the archive before writing when its declared sizes pass a limit.
static void
checkZipLimits(const ZipArchive& archive, const std::wstring& zipFilename,
    const std::wstring& rootpath, const ExtractionLimits& limits);
//=============================================================================
void
UnZip(const std::wstring& zipFilename, const std::wstring& rootpath, wstringVector& filenames)
{
    UnZip(zipFilename, rootpath, filenames, ExtractionLimits());
}
//=============================================================================
void
UnZip(const std::wstring& zipFilename, const std::wstring& rootpath, wstringVector& filenames,
    const ExtractionLimits& limits)
{
    // Open the archive first: a missing archive must not create the destination folder.
    std::optional<ZipArchive> opened;
    try {
        opened.emplace(ZipArchive::openFile(zipFilename, ZipArchive::Mode::Read));
    } catch (const ZipArchiveError& error) {
        if (error.code() == ZipErrorCode::OverlappingEntries) {
            Error(L"Nelson:io:zip:overlappingEntries",
                L"ZIP entries share the same data (zip bomb): %s", utf8_to_wstring(error.entry()));
        }
    }
    if (!opened) {
        Error(L"Nelson:file_archiver:cannotReadFile", L"Cannot read file: %s", zipFilename);
    }
    ZipArchive& archive = *opened;
    checkZipLimits(archive, zipFilename, rootpath, limits);
    if (!FileSystemWrapper::Path::is_directory(rootpath)) {
        FileSystemWrapper::Path p(rootpath);
        std::string errorMessage;
        if (!FileSystemWrapper::Path::create_directories(p, errorMessage)) {
            Error(L"Nelson:unzip:cannotCreateDirectory", L"Cannot create directory.");
        }
    }
    const std::wstring fullRootPath = getRootPath(rootpath);
    const std::wstring canonicalRoot = canonicalFolder(fullRootPath);
    ArchiveDiskWriter disk;
    const auto& entries = archive.entries();
    for (size_t i = 0; i < entries.size(); ++i) {
        ArchiveInterruption::throwIfInterrupted();
        const std::wstring name = utf8_to_wstring(entries[i].name);
        std::wstring safeEntryPath;
        if (!resolveSafeZipEntryPath(fullRootPath, name, safeEntryPath)) {
            Error(L"Nelson:unzip:unsafeEntryPath", L"Unsafe entry path in archive: %s", name);
        }
        if (!name.empty() && name.back() == L'/') {
            try {
                disk.folder(diskPath(canonicalRoot, fullRootPath, safeEntryPath),
                    entries[i].modified, 0755);
            } catch (const ZipArchiveError&) {
                Error(L"Nelson:unzip:cannotCreateDirectory", L"Cannot create directory.");
            }
            filenames.push_back(safeEntryPath);
        } else {
            const std::wstring destination = std::filesystem::path(safeEntryPath).generic_wstring();
            filenames.push_back(destination);
            extractFileEntry(archive, i, disk, diskPath(canonicalRoot, fullRootPath, destination),
                destination);
        }
    }
    try {
        disk.finish();
    } catch (const ZipArchiveError&) {
        Error(L"Nelson:unzip:cannotWriteFile", L"Cannot write extracted file: %s", fullRootPath);
    }
}
//=============================================================================
static void
extractFileEntry(ZipArchive& archive, size_t index, ArchiveDiskWriter& disk,
    const std::wstring& onDisk, const std::wstring& destination)
{
    const ZipEntryInfo& entry = archive.entries()[index];
    if (entry.isEncrypted) {
        Error(L"Nelson:unzip:encryptedEntry", L"Encrypted entries are not supported: %s",
            utf8_to_wstring(entry.name));
    }
    if (!ZipArchive::supportsCompression(entry.method)) {
        Error(L"Nelson:unzip:unsupportedCompressionMethod",
            L"Unsupported compression method %d for entry: %s", static_cast<int>(entry.method),
            utf8_to_wstring(entry.name));
    }
    try {
        disk.beginFile(onDisk, entry.size, entry.modified, diskMode(entry));
    } catch (const ZipArchiveError&) {
        Error(L"Nelson:unzip:cannotOpenDestinationFile", L"Cannot open destination file.");
    }
    bool writeFailed = false;
    bool readFailed = false;
    try {
        archive.readEntry(index, [&](const uint8_t* data, size_t size) {
            ArchiveInterruption::throwIfInterrupted();
            try {
                disk.write(data, size);
            } catch (const ZipArchiveError&) {
                writeFailed = true;
                throw;
            }
        });
        disk.endFile();
    } catch (const ZipArchiveError&) {
        readFailed = !writeFailed;
        disk.abandonFile();
    }
    if (writeFailed) {
        Error(L"Nelson:unzip:cannotWriteFile", L"Cannot write extracted file: %s", destination);
    }
    if (readFailed) {
        Error(L"Nelson:unzip:cannotReadData", L"Cannot read data.");
    }
}
//=============================================================================
static uint32_t
diskMode(const ZipEntryInfo& entry)
{
    // Host system in the high byte: 3 is Unix, 19 is macOS.
    const auto hostSystem = static_cast<uint8_t>(entry.versionMadeBy >> 8);
    const bool unixEntry = hostSystem == 3 || hostSystem == 19;
    return 0644 | (unixEntry ? (entry.externalAttributes >> 16) & 0111 : 0);
}
//=============================================================================
static void
checkZipLimits(const ZipArchive& archive, const std::wstring& zipFilename,
    const std::wstring& rootpath, const ExtractionLimits& limits)
{
    // Entries cannot write more than they declare (checked while reading).
    try {
        ExtractionGuard guard(limits, rootpath);
        uint64_t total = 0;
        for (const ZipEntryInfo& entry : archive.entries()) {
            if (!entry.isDirectory) {
                guard.checkRatio(entry.size, entry.compressedSize, entry.name);
                total += entry.size;
            }
        }
        guard.checkTotal(total, wstring_to_utf8(zipFilename));
    } catch (const ExtractionLimitError& error) {
        raiseExtractionLimitError(error);
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
