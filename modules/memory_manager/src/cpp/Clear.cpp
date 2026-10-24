//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Clear.hpp"
#include "characters_encoding.hpp"
#include "ClearHandle.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
ClearVariable(Evaluator* eval, const std::wstring& variable)
{
    return ClearVariable(eval, wstring_to_utf8(variable));
}
//=============================================================================
bool
ClearVariable(Evaluator* eval, const std::string& variable)
{
    Scope* currentScope = eval->getContext()->getCurrentScope();
    if (!currentScope) {
        return false;
    }
    if (currentScope->isVariableGlobal(variable)) {
        // 'clear g' only removes the local link to the global variable;
        // the global value itself survives ('clear global g' destroys it).
        currentScope->deleteGlobalVariablePointer(variable);
        currentScope->deleteVariable(variable);
        eval->getContext()->notifyVariablesCleared();
        return true;
    }
    ClearHandle(eval, currentScope, variable);
    bool deleted = currentScope->deleteVariable(variable);
    if (deleted) {
        eval->getContext()->notifyVariablesCleared();
    }
    return deleted;
}
//=============================================================================
bool
ClearAllVariables(Evaluator* eval)
{
    bool bUnlocked = true;
    stringVector names;
    eval->getContext()->getCurrentScope()->getVariablesList(true, names);
    for (const auto& name : names) {
        // ClearVariable returns true on success: the returned value reflects
        // whether all variables could be removed (e.g. none was locked).
        if (!ClearVariable(eval, name)) {
            bUnlocked = false;
        }
    }
    return bUnlocked;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
