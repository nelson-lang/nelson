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
#include "IsDirectory.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
IsDirectory(const std::wstring& str)
{
    std::filesystem::path data_dir = createFileSystemPath(str);
    bool bRes = false;
    try {
        bRes = std::filesystem::is_directory(data_dir);
    } catch (const std::filesystem::filesystem_error& e) {
        if (e.code() == std::errc::permission_denied) {
            Error(_W("Permission denied."));
        }
        bRes = false;
    }
    return bRes;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
