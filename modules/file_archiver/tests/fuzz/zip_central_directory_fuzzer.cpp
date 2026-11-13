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
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include "ZipCentralDirectory.hpp"
//=============================================================================
// Any bytes given to the ZIP central directory reader: it lists entries or throws
// ZipArchiveError, it never reads outside the data, loops forever or crashes.
extern "C" int
LLVMFuzzerTestOneInput(const uint8_t* data, size_t size)
{
    try {
        const std::vector<Nelson::ZipDirectoryEntry> entries = Nelson::readZipCentralDirectory(
            [data, size](uint64_t offset, uint8_t* buffer, size_t count) -> size_t {
                if (offset >= size) {
                    return 0;
                }
                const size_t available = std::min<size_t>(count, size - offset);
                std::memcpy(buffer, data + offset, available);
                return available;
            },
            size);
        // Every listed entry must be consistent with the data it came from.
        for (const Nelson::ZipDirectoryEntry& entry : entries) {
            if (entry.nameLength > size) {
                std::abort();
            }
        }
    } catch (const Nelson::ZipArchiveError&) {
    }
    return 0;
}
//=============================================================================
