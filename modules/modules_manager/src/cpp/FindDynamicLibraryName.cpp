//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FindDynamicLibraryName.hpp"
#include <boost/algorithm/string.hpp>
#include <boost/filesystem.hpp>
#include "FileSystemHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
FindDynamicLibraryName(
    const std::wstring& directoryName, const std::wstring& initialLibraryName, bool bCaseSensitive)
{
    std::wstring res;
    boost::filesystem::directory_iterator end_iter;
    boost::filesystem::path dir = directoryName;
    if (!boost::filesystem::is_directory(dir)) {
        return res;
    }
    boost::filesystem::path fullfilename = directoryName;
    fullfilename /= initialLibraryName;
    bool bRes = isFile(fullfilename.native());
    if (bRes) {
        res = initialLibraryName;
        return res;
    }
    for (boost::filesystem::directory_iterator dir_iter(dir.native()); dir_iter != end_iter;
         ++dir_iter) {
        boost::filesystem::path current = dir_iter->path();
        if (boost::filesystem::is_regular_file(current)) {
            if (bCaseSensitive) {
                if (initialLibraryName.compare(current.generic_wstring()) == 0) {
                    return current.generic_wstring();
                }
            } else {
                std::wstring currentfilename = current.filename().generic_wstring();
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
