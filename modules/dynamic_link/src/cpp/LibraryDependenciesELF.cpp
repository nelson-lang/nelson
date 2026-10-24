//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NativeLibraryReader.hpp"
//=============================================================================
namespace Nelson::NativeLibrary {
Information
inspectELF(Reader& file)
{
    const auto elfClass = file.integer(4, 1);
    const auto endian = file.integer(5, 1);
    require((elfClass == 1 || elfClass == 2) && (endian == 1 || endian == 2),
        "Unsupported ELF layout.");
    const bool wide = elfClass == 2;
    const bool big = endian == 2;
    const size_t word = wide ? 8 : 4;
    const auto machine = file.integer(18, 2, big);
    const auto headers = file.integer(wide ? 32 : 28, word, big);
    const auto headerWidth = file.integer(wide ? 54 : 42, 2, big);
    const auto count = file.integer(wide ? 56 : 44, 2, big);
    require(count > 0 && count <= 4096 && headerWidth >= (wide ? 56 : 32),
        "Invalid ELF program headers.");
    file.range(headers, count * headerWidth);
    std::vector<Mapping> mappings;
    uint64_t dynamicOffset = 0;
    uint64_t dynamicSize = 0;
    for (uint64_t i = 0; i < count; ++i) {
        const auto header = headers + i * headerWidth;
        const auto type = file.integer(header, 4, big);
        const auto offset = file.integer(header + (wide ? 8 : 4), word, big);
        const auto address = file.integer(header + (wide ? 16 : 8), word, big);
        const auto size = file.integer(header + (wide ? 32 : 16), word, big);
        file.range(offset, size);
        if (type == 1) {
            mappings.push_back({ address, offset, size });
        }
        if (type == 2) {
            require(dynamicSize == 0, "Duplicate ELF dynamic segment.");
            dynamicOffset = offset;
            dynamicSize = size;
        }
    }
    Information result;
    result.format = "ELF";
    result.architecture = machine == 62 ? "x86_64"
        : machine == 183                ? "aarch64"
        : machine == 3                  ? "x86"
                                        : "unknown";
    if (dynamicSize == 0) {
        return result;
    }
    require(dynamicSize / (2 * word) <= 65536, "ELF dynamic segment is too large.");
    std::vector<std::pair<uint64_t, uint64_t>> strings;
    uint64_t stringAddress = 0;
    uint64_t stringSize = 0;
    bool terminated = false;
    for (uint64_t entry = 0; entry + 2 * word <= dynamicSize; entry += 2 * word) {
        const auto tag = file.integer(dynamicOffset + entry, word, big);
        const auto value = file.integer(dynamicOffset + entry + word, word, big);
        if (tag == 0) {
            terminated = true;
            break;
        }
        if (tag == 5) {
            stringAddress = value;
        }
        if (tag == 10) {
            stringSize = value;
        }
        if (tag == 1 || tag == 14 || tag == 15 || tag == 29 || tag == 0x7fffffff
            || tag == 0x7ffffffd) {
            strings.emplace_back(tag, value);
        }
    }
    require(terminated, "Unterminated ELF dynamic segment.");
    if (strings.empty()) {
        return result;
    }
    const auto table = resolve(file, mappings, stringAddress, stringSize);
    for (const auto& entry : strings) {
        require(entry.second < stringSize, "Invalid ELF dynamic string offset.");
        const auto value = file.string(table + entry.second, stringSize - entry.second,
            entry.first == 15 || entry.first == 29);
        if (entry.first == 14) {
            result.identity = value;
        } else if (entry.first == 15) {
            result.rpaths.push_back(value);
        } else if (entry.first == 29) {
            result.runpaths.push_back(value);
        } else {
            result.needed.push_back(value);
        }
    }
    result.searchPaths = result.runpaths.empty() ? result.rpaths : result.runpaths;
    return result;
}
//=============================================================================
} // namespace Nelson::NativeLibrary
//=============================================================================
