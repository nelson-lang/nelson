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
#include <vector>
#include "TarArchive.hpp"
#include "nlsFile_archiver_exports.h"
//=============================================================================
// Open tar archives of the nelson.io.tar API, referenced by numeric ids.
namespace Nelson::TarArchiveSessions {
//=============================================================================
NLSFILE_ARCHIVER_IMPEXP uint64_t
openFile(const std::wstring& filename, const TarReadOptions& options = TarReadOptions());
NLSFILE_ARCHIVER_IMPEXP uint64_t
openMemory(std::vector<uint8_t> bytes, const TarReadOptions& options = TarReadOptions());
// Empty filename: archive written in memory.
NLSFILE_ARCHIVER_IMPEXP uint64_t
create(const std::wstring& filename, TarCompression compression, int level, int threads = 0);
// Throws ZipArchiveError(InvalidState) for an unknown or closed id.
NLSFILE_ARCHIVER_IMPEXP TarArchive&
get(uint64_t id);
// Adds a file, or a folder with its files and empty subfolders; returns the entry names.
NLSFILE_ARCHIVER_IMPEXP std::vector<std::string>
addPath(uint64_t id, const std::wstring& path, const std::string& entryName);
// Adds data under a new name.
NLSFILE_ARCHIVER_IMPEXP void
addBuffer(uint64_t id, const std::string& name, const std::vector<uint8_t>& data, time_t modified);
// An entry written through a file identifier: begin reserves the name, finish adds the
// spooled file and runs a pending closeWhenIdle.
NLSFILE_ARCHIVER_IMPEXP void
beginEntry(uint64_t id, const std::string& name);
NLSFILE_ARCHIVER_IMPEXP void
finishEntry(uint64_t id, const std::string& name, const std::wstring& spooledFile);
// Closes and forgets the archive; returns the bytes of an in-memory writer.
// Throws EntryStillOpen while an entry is written through a file identifier.
NLSFILE_ARCHIVER_IMPEXP std::vector<uint8_t>
close(uint64_t id);
// Closes now, or when the entries being written are finished.
NLSFILE_ARCHIVER_IMPEXP void
closeWhenIdle(uint64_t id);
//=============================================================================
} // namespace Nelson::TarArchiveSessions
//=============================================================================
