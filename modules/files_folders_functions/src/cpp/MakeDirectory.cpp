//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "MakeDirectory.hpp"
#include "FileSystemWrapper.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
MakeDirectory(const std::wstring& parentDir, const std::wstring& newDir, std::wstring& message,
    std::wstring& messageId)
{
    FileSystemWrapper::Path fullpath = parentDir;
    fullpath /= newDir;
    return MakeDirectory(fullpath.wstring(), message, messageId);
}
//=============================================================================
bool
MakeDirectory(const std::wstring& newDir, std::wstring& message, std::wstring& messageId)
{
    bool bOK = false;
    message = L"";
    messageId = L"";
    if (FileSystemWrapper::Path::is_directory(newDir)) {
        bOK = true;
        message = _W("Directory already exists.");
        messageId = L"Nelson:mkdir:directoryExists";
    } else {
        std::string errorMessage;
        bOK = FileSystemWrapper::Path::create_directories(newDir, errorMessage);
        if (!bOK) {
            message = utf8_to_wstring(errorMessage);
            messageId = L"Nelson:mkdir:OSError";
        }
    }
    return bOK;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
