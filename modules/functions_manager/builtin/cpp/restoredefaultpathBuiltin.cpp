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
#include "restoredefaultpathBuiltin.hpp"
#include "Error.hpp"
#include "ModulesManager.hpp"
#include "PathFuncManager.hpp"
#include <boost/filesystem.hpp>
#include <boost/filesystem/path.hpp>
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FunctionsGateway::restoredefaultpathBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 0, 0);
    PathFuncManager::getInstance()->clearUserPath();
    PathFuncManager::getInstance()->resetUserPath();
    PathFuncManager::getInstance()->clear();
    wstringVector paths = ModulesManager::Instance().getModulesPathList(false);
    for (const auto& path : paths) {
        std::wstring _path = path + L"/functions/";
        boost::filesystem::path data_dir(_path);
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
            PathFuncManager::getInstance()->addPath(_path, true, false);
        }
    }
    return retval;
}
//=============================================================================
