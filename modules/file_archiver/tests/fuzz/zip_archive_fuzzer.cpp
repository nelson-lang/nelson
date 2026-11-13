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
#include <vector>
#include "ZipArchive.hpp"
//=============================================================================
// Any bytes opened as a ZIP archive, then every entry read (with and without a password):
// only ZipArchiveError may come out.
extern "C" int
LLVMFuzzerTestOneInput(const uint8_t* data, size_t size)
{
    constexpr size_t maxEntries = 64;
    constexpr uint64_t maxEntrySize = 1 << 20;
    for (const char* password : { "", "secret" }) {
        try {
            Nelson::ZipArchive archive
                = Nelson::ZipArchive::openMemory(std::vector<uint8_t>(data, data + size));
            archive.setPassword(password);
            const size_t count = std::min(archive.entries().size(), maxEntries);
            for (size_t k = 0; k < count; ++k) {
                try {
                    archive.readEntry(k, maxEntrySize);
                } catch (const Nelson::ZipArchiveError&) {
                }
            }
        } catch (const Nelson::ZipArchiveError&) {
        }
    }
    return 0;
}
//=============================================================================
