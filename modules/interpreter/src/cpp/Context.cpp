//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Context.hpp"
#include "ArrayOf.hpp"
#include "RecursionStack.hpp"
#include "characters_encoding.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
Context::Context()
{
    bypassstack.reserve(1024);
    currentrecursiondepth = DEFAULT_RECURSION_FUNCTION_CALL;
    scopestack.reserve(DEFAULT_RECURSION_FUNCTION_CALL);
    scopestack.emplace_back(new Scope("global"));
    scopestack.emplace_back(new Scope("base"));
}
//=============================================================================
Context::~Context()
{
    while (!scopestack.empty()) {
        delete scopestack.back();
        scopestack.pop_back();
    }
}
//=============================================================================
void
Context::insertVariableLocally(const std::string& varName, const ArrayOf& var)
{
    scopestack.back()->insertVariable(varName, var);
}
//=============================================================================
void
Context::removeVariableLocally(const std::string& varName)
{
    scopestack.back()->deleteVariable(varName);
}
//=============================================================================
bool
Context::insertVariable(const std::string& varName, const ArrayOf& var)
{
    Scope* backScope = scopestack.back();
    bool isPersistent = backScope->isVariablePersistent(varName);
    bool isGlobal = !isPersistent && backScope->isVariableGlobal(varName);
    if (!isPersistent && !isGlobal) {
        return backScope->insertVariable(varName, var);
    }
    Scope* frontScope = scopestack.front();
    if (isPersistent) {
        return frontScope->insertVariable(backScope->getMangledName(varName), var);
    }
    return frontScope->insertVariable(varName, var);
}
//=============================================================================
bool
Context::insertVariableWithoutNameValidation(const std::string& varName, const ArrayOf& var)
{
    Scope* backScope = scopestack.back();
    bool isPersistent = backScope->isVariablePersistent(varName);
    bool isGlobal = !isPersistent && backScope->isVariableGlobal(varName);
    if (!isPersistent && !isGlobal) {
        return backScope->insertVariableWithoutNameValidation(varName, var);
    }
    Scope* frontScope = scopestack.front();
    if (isPersistent) {
        return frontScope->insertVariableWithoutNameValidation(
            backScope->getMangledName(varName), var);
    }
    return frontScope->insertVariableWithoutNameValidation(varName, var);
}
//=============================================================================
ArrayOf*
Context::lookupVariable(const std::string& varName)
{
    Scope* back = scopestack.back();
    bool isPersistent = back->isVariablePersistent(varName);
    bool isGlobal = !isPersistent && back->isVariableGlobal(varName);
    if (!isPersistent && !isGlobal) {
        return back->lookupVariable(varName);
    }
    Scope* front = scopestack.front();
    if (isPersistent) {
        return front->lookupVariable(back->getMangledName(varName));
    }
    return front->lookupVariable(varName);
}
//=============================================================================
bool
Context::lookupVariable(const std::string& varName, ArrayOf& var)
{
    ArrayOf* res = lookupVariable(varName);
    if (res != nullptr) {
        var = *res;
        return true;
    }
    return false;
}
//=============================================================================
bool
Context::isVariable(const std::wstring& varname)
{
    return isVariable(wstring_to_utf8(varname));
}
//=============================================================================
bool
Context::isVariable(const std::string& varname)
{
    Scope* back = scopestack.back();
    bool isPersistent = back->isVariablePersistent(varname);
    bool isGlobal = !isPersistent && back->isVariableGlobal(varname);
    if (!isPersistent && !isGlobal) {
        return back->isVariable(varname);
    }
    Scope* front = scopestack.front();
    if (isPersistent) {
        return front->isVariable(back->getMangledName(varname));
    }
    return front->isVariable(varname);
}
//=============================================================================
bool
Context::isVariableGlobal(const std::string& varName)
{
    return scopestack.back()->isVariableGlobal(varName);
}
//=============================================================================
bool
Context::isVariablePersistent(const std::string& varName)
{
    return scopestack.back()->isVariablePersistent(varName);
}
//=============================================================================
bool
Context::lookupVariableLocally(const std::string& varName, ArrayOf& var)
{
    return scopestack.back()->lookupVariable(varName, var);
}
//=============================================================================
void
Context::insertMacroFunctionLocally(FunctionDefPtr f)
{
    scopestack.back()->insertMacroFunctionLocally(f);
}
//=============================================================================
void
Context::removeMacroFunctionLocally(const std::string& funcName)
{
    scopestack.back()->removeMacroFunctionLocally(funcName);
}
//=============================================================================
void
Context::printMe()
{
}
//=============================================================================
void
Context::addPersistentVariable(const std::string& var)
{
    Scope* back = scopestack.back();
    Scope* front = scopestack.front();
    if (back->isVariableGlobal(var)) {
        Error(L"Nelson:interpreter:globalAndPersistent",
            L"A variable cannot be both global and persistent.");
    }
    back->deleteVariable(var);
    // A persistent declaration shadows a same-scope global declaration, but it
    // must not delete a same-named global variable owned by another scope.
    back->deleteGlobalVariablePointer(var);
    back->addPersistentVariablePointer(var);
    std::string mangledName = back->getMangledName(var);
    ArrayOf v;
    if (!front->lookupVariable(mangledName, v)) {
        front->insertVariable(mangledName, ArrayOf::empty());
    }
}
//=============================================================================
void
Context::addGlobalVariable(const std::string& var)
{
    Scope* back = scopestack.back();
    Scope* front = scopestack.front();
    if (back->isVariablePersistent(var)) {
        Error(L"Nelson:interpreter:globalAndPersistent",
            L"A variable cannot be both global and persistent.");
    }
    back->deleteVariable(var);
    // Delete global persistent variables with this name
    front->deleteVariable(back->getMangledName(var));
    back->addGlobalVariablePointer(var);
    ArrayOf v;
    if (!front->lookupVariable(var, v)) {
        front->insertVariable(var, ArrayOf::empty());
    }
}
//=============================================================================
void
Context::deleteVariable(const std::string& var)
{
    if (isVariableGlobal(var)) {
        scopestack.front()->deleteVariable(var);
        scopestack.back()->deleteGlobalVariablePointer(var);
        return;
    }
    if (isVariablePersistent(var)) {
        scopestack.front()->deleteVariable(scopestack.back()->getMangledName(var));
        scopestack.back()->deletePersistentVariablePointer(var);
        return;
    }
    scopestack.back()->deleteVariable(var);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
