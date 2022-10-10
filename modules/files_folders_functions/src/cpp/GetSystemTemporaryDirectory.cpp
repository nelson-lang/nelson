//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <filesystem>
#include <boost/algorithm/string/predicate.hpp>
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
        tempDir = pwd.generic_wstring();
        if (!boost::algorithm::ends_with(tempDir, L"\\")
            && (!boost::algorithm::ends_with(tempDir, L"/"))) {
            tempDir.append(L"/");
        }
    }
    return tempDir;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
