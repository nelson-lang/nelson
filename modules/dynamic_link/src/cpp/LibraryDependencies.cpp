//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "LibraryDependencies.hpp"
#include "NativeLibraryReader.hpp"
#include "characters_encoding.hpp"
#include "nlohmann/json.hpp"
//=============================================================================
namespace Nelson {
std::string
inspectLibraryDependencies(const std::wstring& filename)
{
    NativeLibrary::Reader file(std::filesystem::u8path(wstring_to_utf8(filename)));
    const auto magic = file.integer(0, 4);
    NativeLibrary::Information result;
    if ((magic & 0xffff) == 0x5a4d) {
        result = NativeLibrary::inspectPE(file);
    } else if (magic == 0x464c457f) {
        result = NativeLibrary::inspectELF(file);
    } else {
        result = NativeLibrary::inspectMachO(file);
    }
    const nlohmann::json document { { "format", result.format },
        { "architecture", result.architecture }, { "identity", result.identity },
        { "needed", result.needed }, { "delayed", result.delayed },
        { "searchPaths", result.searchPaths }, { "rpaths", result.rpaths },
        { "runpaths", result.runpaths } };
    return document.dump();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
