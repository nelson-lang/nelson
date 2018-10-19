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
#include <boost/filesystem.hpp>
using namespace boost::filesystem;
#include "ChangeDirectory.hpp"
#include "Error.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
Cd(std::wstring newpath)
{
    path previous_pwd = current_path();
    try {
        current_path(newpath);
    } catch (const boost::filesystem::filesystem_error& e) {
        e.what();
        Error(_W("Cannot change directory: '") + newpath + L"'.");
    }
    return ArrayOf::characterArrayConstructor(previous_pwd.generic_wstring());
}
//=============================================================================
ArrayOf
Cd(std::string newpath)
{
    path previous_pwd = current_path();
    try {
        current_path(newpath);
    } catch (const boost::filesystem::filesystem_error& e) {
        e.what();
        Error(_("Cannot change directory '") + newpath + "'.");
    }
    return ArrayOf::characterArrayConstructor(previous_pwd.generic_string());
}
//=============================================================================
bool
ChangeDirectory(std::wstring newpath)
{
    try {
        current_path(newpath);
    } catch (const boost::filesystem::filesystem_error& e) {
        e.what();
        return false;
    }
    return true;
}
//=============================================================================
}
//=============================================================================
