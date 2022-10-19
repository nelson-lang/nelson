//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "RemoveDirectory.hpp"
#include "FileSystemHelpers.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
RemoveDirectory(const std::wstring& folderName, bool bSubfolder, std::wstring& message)
{
    bool res = false;
    message = L"";
    bool permissionDenied;
    if (isDirectory(folderName, permissionDenied)) {
        updateFilePermissionsToWrite(folderName);
        Nelson::FileSystemWrapper::Path p(folderName);
        std::string errorMessage;
        if (bSubfolder) {
            if (!Nelson::FileSystemWrapper::Path::remove_all(p, errorMessage)) {
                res = false;
                message = utf8_to_wstring(errorMessage);
                return res;
            }
        }
        res = Nelson::FileSystemWrapper::Path::remove(p, errorMessage);
        if (!res) {
            message = utf8_to_wstring(errorMessage);
        }
    } else {
        if (permissionDenied) {
            message = _W("Permission denied.");
        } else {
            message = _W("an existing directory expected.");
        }
        res = false;
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
