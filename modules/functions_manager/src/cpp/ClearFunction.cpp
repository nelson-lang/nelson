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
#include "ClearFunction.hpp"
#include "BuiltInFunctionDefManager.hpp"
#include "MacroFunctionDef.hpp"
#include "PathFuncManager.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
ClearBuiltin(std::wstring builtinName)
{
    std::string funcName = wstring_to_utf8(builtinName);
    return BuiltInFunctionDefManager::getInstance()->remove(funcName);
}
//=============================================================================
bool
ClearAllBuiltin()
{
    return BuiltInFunctionDefManager::getInstance()->removeAll();
}
//=============================================================================
bool
ClearMacroCache(Evaluator* eval)
{
    stringVector exceptedFunctionsName = eval->getCallers(true);
    PathFuncManager::getInstance()->clearCache(exceptedFunctionsName);
    return true;
}
//=============================================================================
bool
ClearMacroCache()
{
    PathFuncManager::getInstance()->clearCache();
    return true;
}
//=============================================================================
}
//=============================================================================
