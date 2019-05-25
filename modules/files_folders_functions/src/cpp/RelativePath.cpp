//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "RelativePath.hpp"
#include <boost/filesystem.hpp>
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
RelativePath(const std::wstring& path1, const std::wstring& path2, bool& bSuccess)
{
    bSuccess = false;
    boost::filesystem::path pathOne(path1);
    boost::filesystem::path pathTwo(path2);
    boost::filesystem::path relativepath;
    pathOne = pathOne.lexically_normal();
    pathTwo = pathTwo.lexically_normal();
    relativepath = pathTwo.lexically_relative(pathOne);
    std::wstring result = relativepath.generic_wstring();
    if (result.empty()) {
        result = pathTwo.generic_wstring();
        bSuccess = false;
    } else {
        bSuccess = true;
    }
    return result;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
