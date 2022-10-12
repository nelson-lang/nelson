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
#include "MakeDirectory.hpp"
#include "IsDirectory.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
MakeDirectory(const std::wstring& parentDir, const std::wstring& newDir, std::wstring& message)
{
    std::filesystem::path fullpath = createFileSystemPath(parentDir);
    fullpath /= newDir;
    return MakeDirectory(convertFileSytemPathToWString(fullpath), message);
}
//=============================================================================
bool
MakeDirectory(const std::wstring& newDir, std::wstring& message)
{
    bool bOK = false;
    message = L"";
    if (IsDirectory(newDir)) {
        bOK = true;
        message = _W("Directory already exists.");
    } else {
        try {
            std::filesystem::path newDirPath = createFileSystemPath(newDir);
            bOK = std::filesystem::create_directories(newDirPath);
        } catch (const std::filesystem::filesystem_error& e) {
            std::error_code error_code = e.code();
            message = utf8_to_wstring(error_code.message());
        }
    }
    return bOK;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
