//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#if _MSC_VER
#include <Windows.h>
#endif
#include <boost/algorithm/string/predicate.hpp>
#include "FileSystemHelpers.hpp"
#include "GetSystemTemporaryDirectory.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::wstring tempDir;
//=============================================================================
ArrayOf
TempDir()
{
    return ArrayOf::characterArrayConstructor(GetSystemTemporaryDirectory());
}
//=============================================================================
std::wstring
GetSystemTemporaryDirectory()
{
    if (tempDir == L"") {
        std::filesystem::path pwd = std::filesystem::temp_directory_path();
#if _MSC_VER
        std::wstring tempDirTemp = convertFileSytemPathToGenericWString(pwd);
        DWORD length = GetLongPathNameW(tempDirTemp.c_str(), NULL, 0);
        if (length <= 0) {
#ifndef MAX_PATH_LONG
#define MAX_PATH_LONG 32767
#endif
            length = MAX_PATH_LONG;
        }
        std::wstring longPathName;
        longPathName.resize(length + 1);
        length = GetLongPathNameW(tempDirTemp.c_str(), (wchar_t*)longPathName.c_str(), length);
        if (length <= 0) {
            tempDir = convertFileSytemPathToGenericWString(pwd);
        } else {
            tempDir = std::wstring(longPathName);
            tempDir.resize(length);
        }
#else
        tempDir = convertFileSytemPathToGenericWString(pwd);
#endif
        if (!boost::algorithm::ends_with(tempDir, L"\\")
            && !boost::algorithm::ends_with(tempDir, L"/")) {
            tempDir.append(L"/");
        }
    }
    return tempDir;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
