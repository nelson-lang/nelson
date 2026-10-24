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
    void
    imports(Reader& file, const std::vector<Mapping>& sections, uint64_t address, uint64_t length,
        bool delayed, uint64_t imageBase, std::vector<std::string>& names)
    {
        if (address == 0 && length == 0) {
            return;
        }
        const uint64_t width = delayed ? 32 : 20;
        require(address > 0 && length >= width && length / width <= 4096,
            "Invalid PE import directory.");
        const auto offset = resolve(file, sections, address, length);
        for (uint64_t entry = 0; entry + width <= length; entry += width) {
            bool terminator = true;
            for (uint64_t field = 0; field < width; field += 4) {
                terminator = (file.integer(offset + entry + field, 4) == 0) && terminator;
            }
            if (terminator) {
                return;
            }
            auto name = file.integer(offset + entry + (delayed ? 4 : 12), 4);
            if (delayed) {
                const auto flags = file.integer(offset + entry, 4);
                require(flags <= 1, "Unsupported PE delayed import flags.");
                if (flags == 0) {
                    require(name >= imageBase, "Invalid PE delayed import address.");
                    name -= imageBase;
                }
            }
            names.push_back(mappedString(file, sections, name));
        }
        throw std::runtime_error("Unterminated PE import directory.");
    }
    //=============================================================================
} // namespace
//=============================================================================
Information
inspectPE(Reader& file)
{
    const auto header = file.integer(60, 4);
    require(file.integer(header, 4) == 0x4550, "Invalid PE signature.");
    const auto machine = file.integer(header + 4, 2);
    const auto count = file.integer(header + 6, 2);
    require(count > 0 && count <= 96, "Invalid PE section count.");
    const auto optionalSize = file.integer(header + 20, 2);
    const auto optional = header + 24;
    file.range(optional, optionalSize);
    const auto magic = file.integer(optional, 2);
    require(magic == 0x10b || magic == 0x20b, "Unsupported PE optional header.");
    const bool wide = magic == 0x20b;
    const uint64_t directoryOffset = wide ? 112 : 96;
    require(optionalSize >= directoryOffset, "Truncated PE optional header.");
    const auto directories = file.integer(optional + directoryOffset - 4, 4);
    require(directories <= (optionalSize - directoryOffset) / 8, "Invalid PE directory count.");
    const auto headersSize = file.integer(optional + 60, 4);
    file.range(0, headersSize);
    std::vector<Mapping> sections { { 0, 0, headersSize } };
    const auto sectionTable = optional + optionalSize;
    file.range(sectionTable, count * 40);
    for (uint64_t i = 0; i < count; ++i) {
        const auto section = sectionTable + i * 40;
        Mapping mapping { file.integer(section + 12, 4), file.integer(section + 20, 4),
            file.integer(section + 16, 4) };
        file.range(mapping.offset, mapping.length);
        sections.push_back(mapping);
    }
    Information result;
    result.format = "PE";
    result.architecture = machine == 0x8664 ? "x86_64"
        : machine == 0xaa64                 ? "aarch64"
        : machine == 0x14c                  ? "x86"
                                            : "unknown";
    const auto base = file.integer(optional + (wide ? 24 : 28), wide ? 8 : 4);
    if (directories > 1) {
        const auto entry = optional + directoryOffset + 8;
        imports(file, sections, file.integer(entry, 4), file.integer(entry + 4, 4), false, base,
            result.needed);
    }
    if (directories > 13) {
        const auto entry = optional + directoryOffset + 13 * 8;
        imports(file, sections, file.integer(entry, 4), file.integer(entry + 4, 4), true, base,
            result.delayed);
    }
    return result;
}
//=============================================================================
} // namespace Nelson::NativeLibrary
//=============================================================================
