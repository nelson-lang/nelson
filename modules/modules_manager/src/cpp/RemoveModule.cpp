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
#include "RemoveModule.hpp"
#include "Error.hpp"
#include "EvaluateScriptFile.hpp"
#include "ModulesManager.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
RemoveModule(Evaluator* eval, const std::wstring& moduleshortname)
{
    if (IsExistingModuleName(moduleshortname)) {
        bool isProtected = IsProtectedModuleName(moduleshortname);
        if (isProtected && NelsonConfiguration::getInstance()->isModulesProtected()) {
            Warning(L"Nelson:module:protected",
                _W("Cannot remove module (protected): ") + moduleshortname);
            return false;
        }
        std::wstring rootpathmodule = GetModulePath(moduleshortname);
        if (rootpathmodule.empty()) {
            Error(moduleshortname + _W(": This module is registered but it has no path."));
        }
        if (boost::filesystem::is_directory(rootpathmodule)) {
            boost::filesystem::path pathfinish(rootpathmodule);
            pathfinish += L"/etc/finish.nls";
            if (boost::filesystem::exists(pathfinish)
                && !boost::filesystem::is_directory(pathfinish)) {
                EvaluateScriptFile(eval, pathfinish.generic_wstring());
            } else {
                Error(_W("finish.nls does not exist."));
            }
            return UnregisterModule(moduleshortname);
        }
        Error(_W("An existing module root path expected."));
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
