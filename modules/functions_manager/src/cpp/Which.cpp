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
#include "Which.hpp"
#include "BuiltInFunctionDefManager.hpp"
#include "Error.hpp"
#include "GatewayInfo.hpp"
#include "ModulesManager.hpp"
#include "PathFuncManager.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
wstringVector
WhichAll(const std::wstring& functionname)
{
    wstringVector result;
    PathFuncManager::getInstance()->find(functionname, result);
    BuiltInFunctionDefManager::getInstance()->find(wstring_to_utf8(functionname), result);
    return result;
}
//=============================================================================
std::wstring
Which(const std::wstring& functionname)
{
    std::wstring origin = L"";
    if (!PathFuncManager::getInstance()->find(functionname, origin)) {
        BuiltInFunctionDefManager::getInstance()->find(wstring_to_utf8(functionname), origin);
    }
    return origin;
}
//=============================================================================
wstringVector
WhichModule(const std::wstring& functionname)
{
    wstringVector res;
    wstringVector paths = WhichAll(functionname);
    if (paths.size() > 0) {
        for (size_t k = 0; k < paths.size(); k++) {
            std::wstring moduleName = ModulesManager::Instance().findModuleNameByPath(paths[k]);
            if (moduleName != L"") {
                res.push_back(moduleName);
            } else {
                stringVector functionsList;
                std::wstring errorMessage;
                bool bRes = GatewayInfo(paths[k], moduleName, functionsList, errorMessage);
                if (bRes) {
                    res.push_back(moduleName);
                } else {
                    res.push_back(L"");
                }
            }
        }
    }
    return res;
}
//=============================================================================
}
//=============================================================================
