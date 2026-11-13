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
#include <functional>
#include <vector>
#include "ZipArchive.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// One central directory record: what libarchive does not report (sizes, CRC, method,
// attributes) and where the entry starts in the archive.
struct ZipDirectoryEntry
{
    ZipEntryInfo info;
    uint64_t localOffset = 0; // local header position, data before the archive included
    uint64_t nameLength = 0; // stored name length in bytes
};
//=============================================================================
// Reads size bytes at offset, returns how many were read.
using ZipReadAt = std::function<size_t(uint64_t offset, uint8_t* buffer, size_t size)>;
//=============================================================================
// Lists the central directory (ZIP64 and bytes before the archive handled); refuses
// archives whose entries share data.
std::vector<ZipDirectoryEntry>
readZipCentralDirectory(const ZipReadAt& readAt, uint64_t archiveSize);
//=============================================================================
} // namespace Nelson
//=============================================================================
