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
//=============================================================================
namespace {
    Mapping
    selectSlice(Reader& file)
    {
        const auto magic = file.integer(0, 4);
        const bool fat64 = magic == 0xbfbafeca || magic == 0xcafebabf;
        const bool fat32 = magic == 0xbebafeca || magic == 0xcafebabe;
        if (!fat32 && !fat64) {
            return { 0, 0, file.size };
        }
        const bool big = magic == 0xbebafeca || magic == 0xbfbafeca;
        const auto count = file.integer(4, 4, big);
        require(count > 0 && count <= 128, "Invalid Mach-O slice count.");
        const uint64_t width = fat64 ? 32 : 20;
        file.range(8, count * width);
#if defined(_M_ARM64) || defined(__aarch64__)
        constexpr uint64_t host = 0x0100000c;
#else
        constexpr uint64_t host = 0x01000007;
#endif
        for (uint64_t i = 0; i < count; ++i) {
            const auto entry = 8 + i * width;
            const auto offset = file.integer(entry + 8, fat64 ? 8 : 4, big);
            const auto size = file.integer(entry + (fat64 ? 16 : 12), fat64 ? 8 : 4, big);
            file.range(offset, size);
            if (file.integer(entry, 4, big) == host) {
                return { 0, offset, size };
            }
        }
        throw std::runtime_error("No compatible Mach-O slice.");
    }
    //=============================================================================
} // namespace
//=============================================================================
Information
inspectMachO(Reader& file)
{
    const auto slice = selectSlice(file);
    require(slice.length >= 28, "Truncated Mach-O header.");
    const auto magic = file.integer(slice.offset, 4);
    const bool wide = magic == 0xfeedfacf || magic == 0xcffaedfe;
    require(
        wide || magic == 0xfeedface || magic == 0xcefaedfe, "Unsupported native library format.");
    const bool big = magic == 0xcffaedfe || magic == 0xcefaedfe;
    const auto cpu = file.integer(slice.offset + 4, 4, big);
    const auto count = file.integer(slice.offset + 16, 4, big);
    const auto size = file.integer(slice.offset + 20, 4, big);
    const uint64_t headerSize = wide ? 32 : 28;
    require(slice.length >= headerSize && size <= slice.length - headerSize && count <= 65536
            && count <= size / 8,
        "Invalid Mach-O load commands.");
    Information result;
    result.format = "Mach-O";
    result.architecture = cpu == 0x01000007 ? "x86_64"
        : cpu == 0x0100000c                 ? "aarch64"
        : cpu == 7                          ? "x86"
                                            : "unknown";
    uint64_t consumed = 0;
    for (uint64_t i = 0; i < count; ++i) {
        require(size - consumed >= 8, "Truncated Mach-O load command.");
        const auto offset = slice.offset + headerSize + consumed;
        const auto command = file.integer(offset, 4, big) & 0x7fffffff;
        const auto width = file.integer(offset + 4, 4, big);
        require(width >= 8 && width <= size - consumed, "Invalid Mach-O load command size.");
        if (command == 0xc || command == 0xd || command == 0x18 || command == 0x1f
            || command == 0x20 || command == 0x23 || command == 0x1c) {
            const uint64_t minimum = command == 0x1c ? 12 : 24;
            require(width >= minimum, "Truncated Mach-O library command.");
            const auto name = file.integer(offset + 8, 4, big);
            require(name >= minimum && name < width, "Invalid Mach-O library string.");
            const auto value = file.string(offset + name, width - name);
            if (command == 0xd) {
                result.identity = value;
            } else if (command == 0x1c) {
                result.searchPaths.push_back(value);
            } else if (command == 0x18 || command == 0x20) {
                result.delayed.push_back(value);
            } else {
                result.needed.push_back(value);
            }
        }
        consumed += width;
    }
    require(consumed == size, "Unexpected Mach-O command padding.");
    return result;
}
//=============================================================================
} // namespace Nelson::NativeLibrary
//=============================================================================
