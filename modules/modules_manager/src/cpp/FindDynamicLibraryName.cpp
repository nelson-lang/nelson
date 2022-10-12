//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FileSystemHelpers.hpp"
#include <boost/algorithm/string.hpp>
#include "FindDynamicLibraryName.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
FindDynamicLibraryName(
    const std::wstring& directoryName, const std::wstring& initialLibraryName, bool bCaseSensitive)
{
    std::wstring res;
    std::filesystem::directory_iterator end_iter;
    std::filesystem::path dir = createFileSystemPath(directoryName);
    if (!std::filesystem::is_directory(dir)) {
        return res;
    }
    std::filesystem::path fullfilename = directoryName;
    fullfilename /= initialLibraryName;
    bool bRes = false;
    try {
        bRes
            = std::filesystem::exists(fullfilename) && !std::filesystem::is_directory(fullfilename);
    } catch (const std::filesystem::filesystem_error& e) {
        if (e.code() == std::errc::permission_denied) {
            // ONLY FOR DEDUG
        }
        bRes = false;
    }
    if (bRes) {
        res = initialLibraryName;
        return res;
    }
    for (std::filesystem::directory_iterator dir_iter(dir); dir_iter != end_iter; ++dir_iter) {
        std::filesystem::path current = dir_iter->path();
        if (std::filesystem::is_regular_file(current)) {
            if (bCaseSensitive) {
                if (initialLibraryName.compare(convertFileSytemPathToGenericWString(current))
                    == 0) {
                    return convertFileSytemPathToGenericWString(current);
                }
            } else {
                std::wstring currentfilename
                    = convertFileSytemPathToGenericWString(current.filename());
                if (boost::iequals(initialLibraryName, currentfilename)) {
                    return currentfilename;
                }
            }
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
