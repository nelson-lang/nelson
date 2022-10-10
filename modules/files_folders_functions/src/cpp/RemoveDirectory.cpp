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
#include "RemoveDirectory.hpp"
#include "IsDirectory.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static void
updatePermissions(const std::wstring& folderName)
{
    std::filesystem::path f = folderName;
    std::filesystem::permissions(f,
        std::filesystem::perms::owner_write | std::filesystem::perms::group_write
            | std::filesystem::perms::others_write,
        std::filesystem::perm_options::add);
    if (IsDirectory(folderName)) {
        std::filesystem::path branch(folderName);
        for (std::filesystem::recursive_directory_iterator p(branch), end; p != end; ++p) {
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
            std::filesystem::path p = folderName;
            std::filesystem::permissions(p,
                std::filesystem::perms::owner_write | std::filesystem::perms::group_write
                    | std::filesystem::perms::others_write,
                std::filesystem::perm_options::add);
            if (bSubfolder) {
                updatePermissions(p.wstring());
                std::filesystem::remove_all(p);
            }
            std::filesystem::remove(p);
            res = true;
        } catch (const std::filesystem::filesystem_error& e) {
            res = false;
            std::error_code error_code = e.code();
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
