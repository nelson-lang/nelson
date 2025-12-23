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
    if (currentScope->getName() == "base") {
        Scope* globalScope = eval->getContext()->getGlobalScope();
        globalScope->deleteVariable(variable);
    }
    ClearHandle(eval, currentScope, variable);
    return currentScope->deleteVariable(variable);
}
//=============================================================================
bool
ClearAllVariables(Evaluator* eval)
{
    bool bUnlocked = true;
    stringVector names;
    eval->getContext()->getCurrentScope()->getVariablesList(true, names);
    for (const auto& name : names) {
        if (ClearVariable(eval, name)) {
            bUnlocked = false;
        }
    }
    return bUnlocked;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
