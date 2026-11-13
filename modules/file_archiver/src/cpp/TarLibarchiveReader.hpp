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
#include <memory>
#include <string>
#include <vector>
#include "TarArchive.hpp"
//=============================================================================
struct archive;
//=============================================================================
namespace Nelson {
//=============================================================================
// Reads tar archives (any compression libarchive knows), or any archive format libarchive
// reads, one entry at a time.
class TarLibarchiveReader
{
public:
    // Reads memory when it is set, else filename.
    TarLibarchiveReader(std::shared_ptr<const std::vector<uint8_t>> memory,
        const std::wstring& filename, const TarReadOptions& options);
    ~TarLibarchiveReader();
    TarLibarchiveReader(const TarLibarchiveReader&) = delete;
    TarLibarchiveReader&
    operator=(const TarLibarchiveReader&) = delete;
    // Lists files, folders and links; compression and format are the ones found.
    void
    scan(std::vector<TarEntryInfo>& entries, TarCompression& compression, std::string& format);
    // Moves to the data of entry (reopens the archive to go backward).
    void
    openEntry(const TarEntryInfo& entry);
    // Up to size bytes of the current entry, 0 at its end.
    size_t
    read(uint8_t* buffer, size_t size);

private:
    void
    reopen();
    void
    close();
    std::shared_ptr<const std::vector<uint8_t>> memory;
    std::wstring filename;
    TarReadOptions options;
    struct archive* handle = nullptr;
    std::string currentName;
    uint64_t headersRead = 0;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
