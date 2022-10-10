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
#include "RemoveFile.hpp"
#include "IsFile.hpp"
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
    if (IsFile(filename)) {
        std::filesystem::path p = filename;
        try {
            std::filesystem::remove(p);
            res = !IsFile(filename);
        } catch (const std::filesystem::filesystem_error& e) {
            std::error_code error_code = e.code();
            if (e.code() == std::errc::permission_denied) {
                try {
                    std::filesystem::permissions(p,
                        std::filesystem::perms::owner_write | std::filesystem::perms::group_write
                            | std::filesystem::perms::others_write,
                        std::filesystem::perm_options::add);
                    std::filesystem::remove(p);
                    res = !IsFile(filename);
                } catch (const std::filesystem::filesystem_error& e) {
                    error_code = e.code();
                    res = false;
                    message = utf8_to_wstring(error_code.message());
                }
            } else {
                res = false;
                message = utf8_to_wstring(error_code.message());
            }
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
