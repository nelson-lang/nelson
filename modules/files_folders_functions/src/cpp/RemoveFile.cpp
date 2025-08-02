//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "RemoveFile.hpp"
#include "FileSystemWrapper.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
RemoveFile(const std::wstring& filename, std::wstring& message)
{
    bool res = false;
    message = L"";
    bool permissionDenied;
    bool bIsFile = FileSystemWrapper::Path::is_regular_file(filename, permissionDenied);
    if (permissionDenied) {
        FileSystemWrapper::Path p = (filename);
        if (!FileSystemWrapper::Path::updateFilePermissionsToWrite(p)) {
            message = _W("Permission denied");
            return false;
        }
        std::string errorMessage;
        if (!FileSystemWrapper::Path::remove(p, errorMessage)) {
            message = utf8_to_wstring(errorMessage);
            return false;
        }
    }
    if (bIsFile) {
        std::string errorMessage;
        FileSystemWrapper::Path p(filename);
        if (!FileSystemWrapper::Path::remove(p, errorMessage)) {
            message = utf8_to_wstring(errorMessage);
            return false;
        }
    } else {
        res = false;
        message = _W("an existing file expected.");
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
