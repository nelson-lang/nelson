//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "StringHelpers.hpp"
#include "FindDynamicLibraryName.hpp"
#include "FileSystemWrapper.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
FindDynamicLibraryName(
    const std::wstring& directoryName, const std::wstring& initialLibraryName, bool bCaseSensitive)
{
    std::wstring res;
    nfs::directory_iterator end_iter;
    nfs::path dir = directoryName;
    if (!nfs::is_directory(dir)) {
        return res;
    }
    nfs::path fullfilename = directoryName;
    fullfilename /= initialLibraryName;
    bool bRes = nfs::is_regular_file(fullfilename);
    if (bRes) {
        res = initialLibraryName;
        return res;
    }
    for (nfs::directory_iterator dir_iter(dir.native()); dir_iter != end_iter; ++dir_iter) {
        nfs::path current = dir_iter->path();
        if (nfs::is_regular_file(current)) {
            if (bCaseSensitive) {
                if (initialLibraryName.compare(current.generic_wstring()) == 0) {
                    return current.generic_wstring();
                }
            } else {
                std::wstring currentfilename = current.filename().generic_wstring();
                if (StringHelpers::iequals(initialLibraryName, currentfilename)) {
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
