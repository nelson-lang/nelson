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
#include <string>
#include <utility>
#include <vector>
#include "ZipArchive.hpp"
#include "nlsFile_archiver_exports.h"
//=============================================================================
// Open ZIP archives of the nelson.io.zip API, referenced by numeric ids.
namespace Nelson::ZipArchiveSessions {
//=============================================================================
// mode is "r", "w" (replace), "a" (append) or "x" (new file only).
NLSFILE_ARCHIVER_IMPEXP uint64_t
openFile(const std::wstring& filename, const std::string& mode);
NLSFILE_ARCHIVER_IMPEXP uint64_t
openMemory(std::vector<uint8_t> bytes);
NLSFILE_ARCHIVER_IMPEXP uint64_t
createInMemory();
// Throws ZipArchiveError(InvalidState) for an unknown or closed id.
NLSFILE_ARCHIVER_IMPEXP ZipArchive&
get(uint64_t id);
// Items for a file, or a folder with its files and empty subfolders (entryName replaces the
// file or folder name).
NLSFILE_ARCHIVER_IMPEXP std::vector<ZipFileItem>
pathItems(const std::wstring& path, const std::string& entryName, const ZipWriteOptions& options);
// Adds a file, or a folder with its files and empty subfolders; returns the entry names.
NLSFILE_ARCHIVER_IMPEXP std::vector<std::string>
addPath(uint64_t id, const std::wstring& path, const std::string& entryName,
    const ZipWriteOptions& options);
// Closes and forgets the archive; returns the bytes of an in-memory writer.
// Throws EntryStillOpen while an entry is written through a file identifier.
NLSFILE_ARCHIVER_IMPEXP std::vector<uint8_t>
close(uint64_t id);
// Closes now, or when the entry being written is finished (object deleted meanwhile).
NLSFILE_ARCHIVER_IMPEXP void
closeWhenIdle(uint64_t id);
// Ends the entry written through a file identifier and runs a pending closeWhenIdle.
NLSFILE_ARCHIVER_IMPEXP void
finishEntry(uint64_t id);
// Starts an entry written block by block; refuses a name already in the archive.
NLSFILE_ARCHIVER_IMPEXP void
beginEntry(uint64_t id, const std::string& name, const ZipWriteOptions& options);
// Rewrites a ZIP file without the removed entries and with renamed ones (no recompression).
NLSFILE_ARCHIVER_IMPEXP void
rewrite(const std::wstring& filename, const std::vector<std::string>& removed,
    const std::vector<std::pair<std::string, std::string>>& renamed);
//=============================================================================
} // namespace Nelson::ZipArchiveSessions
//=============================================================================
