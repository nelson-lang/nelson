//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GetSystemTemporaryDirectory.hpp"
#include "FileSystemWrapper.hpp"
#include "StringHelpers.hpp"
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
        FileSystemWrapper::Path pwd(FileSystemWrapper::Path::temp_directory_path());
        tempDir = pwd.getFinalPathname().generic_wstring();
        if (!StringHelpers::ends_with(tempDir, L"\\")
            && (!StringHelpers::ends_with(tempDir, L"/"))) {
            tempDir.append(L"/");
        }
    }
    return tempDir;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
