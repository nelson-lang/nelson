//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ClearFunction.hpp"
#include "BuiltInFunctionDefManager.hpp"
#include "MacroFunctionDef.hpp"
#include "PathFunctionIndexerManager.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
ClearBuiltin(const std::wstring& builtinName)
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
    PathFunctionIndexerManager::getInstance()->clearCache(exceptedFunctionsName);
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
