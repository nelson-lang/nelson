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
#include "IsDirectory.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
#include <boost/filesystem.hpp>
//=============================================================================
namespace Nelson {
//=============================================================================
static void
updatePermissions(const std::wstring& folderName)
{
    boost::filesystem::path f = folderName;
    boost::filesystem::permissions(f,
        boost::filesystem::add_perms | boost::filesystem::owner_write
            | boost::filesystem::group_write | boost::filesystem::others_write);
    if (IsDirectory(folderName)) {
        boost::filesystem::path branch(folderName);
        for (boost::filesystem::recursive_directory_iterator p(branch), end; p != end; ++p) {
            updatePermissions(p->path().wstring());
        }
    }
}
//=============================================================================
bool
RemoveDirectory(const std::wstring& folderName, bool bSubfolder, std::wstring& message)
{
    bool res = false;
    message = L"";
    if (IsDirectory(folderName)) {
        try {
            boost::filesystem::path p = folderName;
            boost::filesystem::permissions(p,
                boost::filesystem::add_perms | boost::filesystem::owner_write
                    | boost::filesystem::group_write | boost::filesystem::others_write);
            if (bSubfolder) {
                updatePermissions(p.wstring());
                boost::filesystem::remove_all(p);
            }
            boost::filesystem::remove(p);
            res = true;
        } catch (const boost::filesystem::filesystem_error& e) {
            res = false;
            boost::system::error_code error_code = e.code();
            message = utf8_to_wstring(error_code.message());
        }
    } else {
        res = false;
        message = _W("an existing directory expected.");
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
