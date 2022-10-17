//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "MakeDirectory.hpp"
#include "FileSystemHelpers.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
MakeDirectory(const std::wstring& parentDir, const std::wstring& newDir, std::wstring& message)
{
    Nelson::FileSystemWrapper::Path fullpath = parentDir;
    fullpath /= newDir;
    return MakeDirectory(fullpath.wstring(), message);
}
//=============================================================================
bool
MakeDirectory(const std::wstring& newDir, std::wstring& message)
{
    bool bOK = false;
    message = L"";
    if (isDirectory(newDir)) {
        bOK = true;
        message = _W("Directory already exists.");
    } else {
        try {
            bOK = Nelson::FileSystemWrapper::Path::create_directories(newDir);
        } catch (const boost::filesystem::filesystem_error& e) {
            boost::system::error_code error_code = e.code();
            message = utf8_to_wstring(error_code.message());
        }
    }
    return bOK;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
