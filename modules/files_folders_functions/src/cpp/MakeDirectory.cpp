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
#include "MakeDirectory.hpp"
#include "IsDirectory.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
#include <boost/filesystem.hpp>
//=============================================================================
namespace Nelson {
//=============================================================================
bool
MakeDirectory(const std::wstring& parentDir, const std::wstring& newDir, std::wstring& message)
{
    boost::filesystem::path fullpath = parentDir;
    fullpath /= newDir;
    return MakeDirectory(fullpath.wstring(), message);
}
//=============================================================================
bool
MakeDirectory(const std::wstring& newDir, std::wstring& message)
{
    bool bOK = false;
    message = L"";
    if (IsDirectory(newDir)) {
        bOK = true;
        message = _W("Directory already exists.");
    } else {
        try {
            bOK = boost::filesystem::create_directories(newDir);
        } catch (const boost::filesystem::filesystem_error& e) {
            boost::system::error_code error_code = e.code();
            message = utf8_to_wstring(error_code.message());
        }
    }
    return bOK;
}
//=============================================================================
}
//=============================================================================
