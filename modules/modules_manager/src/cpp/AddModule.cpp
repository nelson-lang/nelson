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
#include "AddModule.hpp"
#include "Error.hpp"
#include "EvaluateScriptFile.hpp"
#include "FindDynamicLibraryName.hpp"
#include "ModulesHelpers.hpp"
#include "ModulesManager.hpp"
#include "characters_encoding.hpp"
#include <boost/filesystem.hpp>
//=============================================================================
namespace Nelson {
//=============================================================================
void
AddModule(Evaluator* eval, std::wstring modulerootpath, std::wstring moduleshortname)
{
    if (boost::filesystem::is_directory(modulerootpath)) {
        boost::filesystem::path pathmainloader(modulerootpath);
        pathmainloader += L"/etc/startup.nls";
        if (boost::filesystem::exists(pathmainloader)
            && !boost::filesystem::is_directory(pathmainloader)) {
            if (!IsExistingModuleName(moduleshortname) && !IsExistingModulePath(modulerootpath)) {
                RegisterModule(moduleshortname, modulerootpath);
                EvaluateScriptFile(eval, pathmainloader.generic_wstring().c_str());
            } else {
                if ((IsExistingModuleName(moduleshortname)
                        && IsExistingModulePath(modulerootpath))) {
                    Error(moduleshortname + _W(": This module is already used."));
                } else {
                    if (IsExistingModuleName(moduleshortname)) {
                        Error(_W("An existing module with the same name already used."));
                    }
                    if (IsExistingModulePath(modulerootpath)) {
                        Error(_W("An existing module with the same path already defined."));
                    }
                }
            }
        } else {
            Error(_W("startup.nls does not exist."));
        }
    } else {
        Error(_W("An existing module root path expected."));
    }
}
//=============================================================================
}
//=============================================================================
