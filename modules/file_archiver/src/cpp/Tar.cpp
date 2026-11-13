//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cstdio>
#include <functional>
#include <filesystem>
#include <optional>
#include "Tar.hpp"
#include "FileSystemWrapper.hpp"
#include "ArchiveDiskWriter.hpp"
#include "ArchiveInterruption.hpp"
#include "ZipHelpers.hpp"
#include "characters_encoding.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static void
extractTarFile(TarArchive& archive, size_t index, ArchiveDiskWriter& disk,
    const std::wstring& onDisk, const std::wstring& destination);
// Symbolic link kept inside the destination folder (not on Windows); false when skipped.
static bool
restoreLink(ArchiveDiskWriter& disk, const TarEntryInfo& entry, const std::wstring& onDisk);
// Runs a disk write, raising the untar write error on failure.
static void
writeOrRaise(const std::wstring& destination, const std::function<void()>& action);
// Refuses the archive before writing when its sizes pass a limit.
static void
checkTarLimits(
    const TarArchive& archive, const std::wstring& rootpath, const ExtractionLimits& limits);
//=============================================================================
void
Tar(const std::wstring& tarFilename, const wstringVector& names, const std::wstring& rootpath,
    wstringVector& entrynames)
{
    if (!rootpath.empty() && !FileSystemWrapper::Path::is_directory(rootpath)) {
        Error(L"Nelson:tar:invalidRootPath", L"Invalid root path.");
    }
    std::vector<ZipFileItem> items;
    collectArchiveItems(names, rootpath, false, items, entrynames);
    if (items.empty()) {
        Error(L"Nelson:tar:nothingToTar", L"Nothing to archive.");
    }
    std::optional<TarArchive> archive;
    try {
        archive.emplace(TarArchive::createFile(
            tarFilename, TarArchive::compressionFromFilename(tarFilename), -1));
        archive->addFiles(items);
        archive->close();
    } catch (const ZipArchiveError& error) {
        // Dropping the writer removes its temporary file.
        archive.reset();
        if (error.code() == ZipErrorCode::OpenFailed && !error.entry().empty()) {
            Error(L"Nelson:file_archiver:cannotReadFile", L"Cannot read file: %s",
                itemFilename(items, error.entry()));
        }
        if (error.code() == ZipErrorCode::UnsupportedCompression) {
            Error(L"Nelson:tar:unsupportedCompression",
                L"This compression is not available in this build: %s", tarFilename);
        }
        Error(L"Nelson:tar:cannotWriteFile", L"Cannot write file: %s", tarFilename);
    }
}
//=============================================================================
void
UnTar(const std::wstring& tarFilename, const std::wstring& rootpath, wstringVector& filenames,
    const ExtractionLimits& limits)
{
    // Open the archive first: a missing archive must not create the destination folder.
    std::optional<TarArchive> archive;
    try {
        archive.emplace(TarArchive::openFile(tarFilename));
    } catch (const ZipArchiveError& error) {
        if (error.code() == ZipErrorCode::NotAZipArchive) {
            Error(L"Nelson:untar:notTarArchive", L"Not a tar archive: %s", tarFilename);
        }
        Error(L"Nelson:file_archiver:cannotReadFile", L"Cannot read file: %s", tarFilename);
    }
    ExtractTar(*archive, rootpath, filenames, limits);
}
//=============================================================================
void
ExtractTar(TarArchive& archive, const std::wstring& rootpath, wstringVector& filenames,
    const ExtractionLimits& limits)
{
    checkTarLimits(archive, rootpath, limits);
    if (!FileSystemWrapper::Path::is_directory(rootpath)) {
        FileSystemWrapper::Path p(rootpath);
        std::string errorMessage;
        if (!FileSystemWrapper::Path::create_directories(p, errorMessage)) {
            Error(L"Nelson:untar:cannotCreateDirectory", L"Cannot create directory.");
        }
    }
    const std::wstring fullRootPath = getRootPath(rootpath);
    const std::wstring canonicalRoot = canonicalFolder(fullRootPath);
    ArchiveDiskWriter disk;
    const auto& entries = archive.entries();
    for (size_t i = 0; i < entries.size(); ++i) {
        ArchiveInterruption::throwIfInterrupted();
        const TarEntryInfo& entry = entries[i];
        const std::wstring name = utf8_to_wstring(entry.name);
        std::wstring safeEntryPath;
        if (!resolveSafeZipEntryPath(fullRootPath, name, safeEntryPath)) {
            Error(L"Nelson:untar:unsafeEntryPath", L"Unsafe entry path in archive: %s", name);
        }
        const std::wstring destination = std::filesystem::path(safeEntryPath).generic_wstring();
        const std::wstring onDisk = diskPath(canonicalRoot, fullRootPath, destination);
        if (entry.isSymlink) {
            if (!restoreLink(disk, entry, onDisk)) {
                continue;
            }
        } else if (entry.isDirectory) {
            writeOrRaise(destination,
                [&] { disk.folder(onDisk, entry.modified, 0755); });
        } else {
            extractTarFile(archive, i, disk, onDisk, destination);
        }
        filenames.push_back(destination);
    }
    writeOrRaise(fullRootPath, [&] { disk.finish(); });
}
//=============================================================================
static void
extractTarFile(TarArchive& archive, size_t index, ArchiveDiskWriter& disk,
    const std::wstring& onDisk, const std::wstring& destination)
{
    const TarEntryInfo& entry = archive.entries()[index];
    // Only the execute bits come from the archive; the umask applies.
    writeOrRaise(destination,
        [&] { disk.beginFile(onDisk, entry.size, entry.modified, 0644 | (entry.mode & 0111)); });
    std::vector<uint8_t> block(64 * 1024);
    try {
        archive.openEntry(index);
        for (size_t count = archive.readEntryData(block.data(), block.size()); count > 0;
            count = archive.readEntryData(block.data(), block.size())) {
            ArchiveInterruption::throwIfInterrupted();
            disk.write(block.data(), count);
        }
    } catch (const ZipArchiveError& error) {
        disk.abandonFile();
        if (error.code() == ZipErrorCode::WriteFailed) {
            Error(L"Nelson:untar:cannotWriteFile", L"Cannot write extracted file: %s",
                destination);
        }
        Error(L"Nelson:untar:cannotReadData", L"Cannot read data.");
    }
    writeOrRaise(destination, [&] { disk.endFile(); });
}
//=============================================================================
static bool
restoreLink(ArchiveDiskWriter& disk, const TarEntryInfo& entry, const std::wstring& onDisk)
{
#ifdef _WIN32
    // Creating links needs privileges on Windows: links are skipped there.
    (void)disk;
    (void)entry;
    (void)onDisk;
    return false;
#else
    if (entry.isHardlink || !linkStaysInside(entry.name, entry.linkName)) {
        return false;
    }
    writeOrRaise(onDisk,
        [&] { disk.link(onDisk, entry.linkName, entry.modified); });
    return true;
#endif
}
//=============================================================================
static void
writeOrRaise(const std::wstring& destination, const std::function<void()>& action)
{
    try {
        action();
    } catch (const ZipArchiveError&) {
        Error(L"Nelson:untar:cannotWriteFile", L"Cannot write extracted file: %s", destination);
    }
}
//=============================================================================
static void
checkTarLimits(
    const TarArchive& archive, const std::wstring& rootpath, const ExtractionLimits& limits)
{
    // Tar headers give exact sizes: the whole extraction is known before writing.
    const std::wstring name = archive.sourceName().empty() ? L"tar data" : archive.sourceName();
    try {
        ExtractionGuard guard(limits, rootpath);
        uint64_t total = 0;
        for (const TarEntryInfo& entry : archive.entries()) {
            if (!entry.isDirectory && !entry.isSymlink) {
                total += entry.size;
            }
        }
        guard.checkRatio(total, archive.sourceSize(), wstring_to_utf8(name));
        guard.checkTotal(total, wstring_to_utf8(name));
    } catch (const ExtractionLimitError& error) {
        raiseExtractionLimitError(error);
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
