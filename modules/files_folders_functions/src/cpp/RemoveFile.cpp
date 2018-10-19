//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
RemoveFile(std::wstring filename, std::wstring& message)
{
    bool res = false;
    message = L"";
    if (IsFile(filename)) {
        try {
            boost::filesystem::path p = filename;
            boost::filesystem::remove(p);
            res = !IsFile(filename);
        } catch (const boost::filesystem::filesystem_error& e) {
            res = false;
            boost::system::error_code error_code = e.code();
            message = utf8_to_wstring(error_code.message());
        }
    } else {
        res = false;
        message = _W("an existing file expected.");
    }
    return res;
}
//=============================================================================
}
//=============================================================================
