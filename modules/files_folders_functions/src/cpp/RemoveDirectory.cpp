//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FileSystemWrapper.hpp"
#include "RemoveDirectory.hpp"
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
    if (FileSystemWrapper::Path::is_directory(folderName, permissionDenied)) {
        FileSystemWrapper::Path::updateFilePermissionsToWrite(folderName);
        FileSystemWrapper::Path p(folderName);
        std::string errorMessage;
        if (bSubfolder) {
            res = FileSystemWrapper::Path::remove_all(p, errorMessage);
            if (!res) {
                message = utf8_to_wstring(errorMessage);
                return res;
            }
        } else {
            res = FileSystemWrapper::Path::remove(p, errorMessage);
        }
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
