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
#include <boost/filesystem.hpp>
#include <boost/filesystem/path.hpp>
#include "NormalizePath.hpp"
#include "userpathBuiltin.hpp"
#include "Error.hpp"
#include "PathFuncManager.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FunctionsGateway::userpathBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() == 1) {
        nargoutcheck(nLhs, 0, 0);
        ArrayOf param1 = argIn[0];
        if (param1.isRowVectorCharacterArray()) {
            std::wstring paramstr = param1.getContentAsWideString();
            if (paramstr == L"clear") {
                PathFuncManager::getInstance()->clearUserPath(true);
            } else if (paramstr == L"reset") {
                PathFuncManager::getInstance()->resetUserPath();
            } else {
                boost::filesystem::path data_dir(paramstr);
                bool bRes = false;
                try {
                    bRes = boost::filesystem::is_directory(data_dir);
                } catch (const boost::filesystem::filesystem_error& e) {
                    if (e.code() == boost::system::errc::permission_denied) {
                        // ONLY FOR DEBUG
                    }
                    bRes = false;
                }
                if (bRes) {
                    PathFuncManager::getInstance()->setUserPath(NormalizePath(paramstr), true);
                } else {
                    Error(_W("Not an existing directory:") + L" " + paramstr);
                }
            }
        } else {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
        }
    } else {
        nargincheck(argIn, 0, 0);
        nargoutcheck(nLhs, 0, 1);
        retval << ArrayOf::characterArrayConstructor(PathFuncManager::getInstance()->getUserPath());
    }
    return retval;
}
//=============================================================================
