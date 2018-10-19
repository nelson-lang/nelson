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
#include "IsFile.hpp"
#include "Error.hpp"
#include <boost/filesystem.hpp>
#include <boost/filesystem/path.hpp>
//=============================================================================
using namespace boost::filesystem;
//=============================================================================
namespace Nelson {
//=============================================================================
bool
IsFile(std::wstring str)
{
    boost::filesystem::path data_dir(str);
    bool bRes = false;
    try {
        bRes = boost::filesystem::exists(data_dir) && !boost::filesystem::is_directory(data_dir);
    } catch (const boost::filesystem::filesystem_error& e) {
        if (e.code() == boost::system::errc::permission_denied) {
            Error(_W("Permission denied."));
        }
        bRes = false;
    }
    return bRes;
}
//=============================================================================
}
//=============================================================================
