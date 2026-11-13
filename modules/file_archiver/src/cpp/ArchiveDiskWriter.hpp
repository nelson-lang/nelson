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
#include <string>
//=============================================================================
struct archive;
//=============================================================================
namespace Nelson {
//=============================================================================
// Writes extracted entries to disk through libarchive: parent folders, dates, Unix mode
// (umask applied, not on Windows); refuses '..' and writing through symbolic links.
// Paths are full paths already checked against the destination folder.
class ArchiveDiskWriter
{
public:
    ArchiveDiskWriter();
    ~ArchiveDiskWriter();
    ArchiveDiskWriter(const ArchiveDiskWriter&) = delete;
    ArchiveDiskWriter&
    operator=(const ArchiveDiskWriter&) = delete;

    void
    folder(const std::wstring& path, time_t modified, uint32_t mode);
    // A file: beginFile, write its blocks, endFile.
    void
    beginFile(const std::wstring& path, uint64_t size, time_t modified, uint32_t mode);
    void
    write(const uint8_t* data, size_t size);
    void
    endFile();
    // Removes the file being written (failed or interrupted extraction).
    void
    abandonFile();
    // Symbolic link to target (relative, already checked to stay in the destination).
    void
    link(const std::wstring& path, const std::string& target, time_t modified);
    // Applies the dates of the folders written.
    void
    finish();

private:
    void
    writeHeader(const std::wstring& path, unsigned int type, uint64_t size, time_t modified,
        uint32_t mode, const std::string& target);
    struct archive* disk = nullptr;
    std::wstring currentFile;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
