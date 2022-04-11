//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "RemoveFile.hpp"
#include "IsFile.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
#include <boost/filesystem.hpp>
//=============================================================================
namespace Nelson {
//=============================================================================
bool
RemoveFile(const std::wstring& filename, std::wstring& message)
{
    bool res = false;
    message = L"";
    if (IsFile(filename)) {
        boost::filesystem::path p = filename;
        try {
            boost::filesystem::remove(p);
            res = !IsFile(filename);
        } catch (const boost::filesystem::filesystem_error& e) {
            boost::system::error_code error_code = e.code();
            if (e.code() == boost::system::errc::permission_denied) {
                try {
                    boost::filesystem::permissions(p,
                        boost::filesystem::add_perms | boost::filesystem::owner_write
                            | boost::filesystem::group_write | boost::filesystem::others_write);
                    boost::filesystem::remove(p);
                    res = !IsFile(filename);
                } catch (const boost::filesystem::filesystem_error& e) {
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
