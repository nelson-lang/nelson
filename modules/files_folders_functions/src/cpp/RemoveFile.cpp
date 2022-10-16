//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <boost/filesystem.hpp>
#include "RemoveFile.hpp"
#include "FileSystemHelpers.hpp"
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
    bool bIsFile = isFile(filename, permissionDenied);
    if (permissionDenied) {
        boost::filesystem::path p = createFileSystemPath(filename);
        try {
            boost::filesystem::permissions(p,
                boost::filesystem::add_perms | boost::filesystem::owner_write
                    | boost::filesystem::group_write | boost::filesystem::others_write);
            boost::filesystem::remove(p);
        } catch (const boost::filesystem::filesystem_error& e) {
            boost::system::error_code error_code = e.code();
            message = utf8_to_wstring(error_code.message());
            return false;
        }
    }
    if (bIsFile) {
        boost::filesystem::path p = createFileSystemPath(filename);
        try {
            boost::filesystem::remove(p);
        } catch (const boost::filesystem::filesystem_error& e) {
            boost::system::error_code error_code = e.code();
            res = false;
            message = utf8_to_wstring(error_code.message());
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
