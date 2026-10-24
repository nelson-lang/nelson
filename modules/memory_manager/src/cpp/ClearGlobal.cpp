//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include "StringHelpers.hpp"
#include "ClearGlobal.hpp"
#include "MacroFunctionDef.hpp"
#include "BytecodeChunk.hpp"
#include "characters_encoding.hpp"
#include "FunctionsInMemory.hpp"
#include "HandleState.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static void
appendUniqueName(stringVector& names, const std::string& name)
{
    if (std::find(names.begin(), names.end(), name) == names.end()) {
        names.push_back(name);
    }
}
//=============================================================================
static void
collectAstPersistentVariables(AbstractSyntaxTreePtr node, stringVector& names)
{
    if (node == nullptr) {
        return;
    }
    if (node->type == non_terminal && node->opNum == OP_SCALL && node->down != nullptr
        && node->down->text == "persistent") {
        AbstractSyntaxTreePtr arg = node->down->right;
        while (arg != nullptr) {
            if (arg->type == id_node && !arg->text.empty()) {
                appendUniqueName(names, arg->text);
            }
            arg = arg->right;
        }
    }
    collectAstPersistentVariables(node->down, names);
    collectAstPersistentVariables(node->right, names);
}
//=============================================================================
static stringVector
collectPersistentVariableNames(MacroFunctionDef* macroFunction)
{
    stringVector names;
    if (macroFunction == nullptr) {
        return names;
    }
    collectAstPersistentVariables(macroFunction->code, names);
    if (macroFunction->ensureBytecodeCompiled() && macroFunction->bytecodeChunk != nullptr) {
        for (const Instruction& ins : macroFunction->bytecodeChunk->code) {
            if (ins.op != OpCode::DECLARE_PERSISTENT) {
                continue;
            }
            if (ins.A >= macroFunction->bytecodeChunk->names.size()) {
                continue;
            }
            appendUniqueName(names, macroFunction->bytecodeChunk->names[ins.A]);
        }
    }
    return names;
}
//=============================================================================
static bool
clearBytecodePersistentVariables(Evaluator* eval, MacroFunctionDef* macroFunction)
{
    bool res = false;
    if (eval == nullptr || macroFunction == nullptr) {
        return res;
    }
    const std::string prefix = "_" + macroFunction->getName() + "_";
    Scope* globalScope = eval->getContext()->getGlobalScope();
    for (const std::string& name : collectPersistentVariableNames(macroFunction)) {
        res = globalScope->deleteVariable(prefix + name) || res;
    }
    return res;
}
//=============================================================================
static bool
callClearHandle(Evaluator* eval, Scope* scope, const std::string& variable)
{
    ArrayOf val;
    if (!scope->lookupVariable(variable, val) || !val.isHandle()) {
        return false;
    }
    const auto* handles = static_cast<const HandleRef*>(val.dataPointer());
    bool res = false;
    for (indexType k = 0; handles != nullptr && k < val.elementCount(); ++k) {
        HandleRef ref = handles[k];
        HandleGenericObject* object = ref ? ref->object() : nullptr;
        if (object != nullptr && object->isScoped()) {
            object->onClear(eval);
            res = true;
        }
    }
    return res;
}
//=============================================================================
bool
ClearGlobalVariable(Evaluator* eval, const std::wstring& variable)
{
    return ClearGlobalVariable(eval, wstring_to_utf8(variable));
}
//=============================================================================
bool
ClearGlobalVariable(Evaluator* eval, const std::string& variable)
{
    Scope* globalScope = eval->getContext()->getGlobalScope();
    callClearHandle(eval, globalScope, variable);
    bool res = eval->getContext()->getGlobalScope()->deleteVariable(variable);
    return res;
}
//=============================================================================
bool
ClearAllGlobalVariables(Evaluator* eval)
{
    bool bUnlocked = true;
    stringVector names;
    eval->getContext()->getGlobalScope()->getVariablesList(true, names);
    for (const auto& name : names) {
        if (!eval->getContext()->getGlobalScope()->deleteVariable(name)) {
            bUnlocked = false;
        }
    }
    return bUnlocked;
}
//=============================================================================
bool
ClearAllPersistentVariables(Evaluator* eval)
{
    bool bUnlocked = true;
    stringVector names;
    Scope* globalScope = eval->getContext()->getGlobalScope();
    globalScope->getVariablesList(true, names);
    for (const auto& name : names) {
        // Persistent variables are stored in the global scope under a mangled
        // name "_<function>_<variable>" (see Scope::getMangledName). User
        // global variables cannot start with '_' (not a valid identifier), so
        // only the mangled persistent entries must be removed here.
        if (StringHelpers::starts_with(name, "_")) {
            if (!globalScope->deleteVariable(name)) {
                bUnlocked = false;
            }
        }
    }
    return bUnlocked;
}
//=============================================================================
bool
ClearPersistentVariable(Evaluator* eval, const std::wstring& variable)
{
    return ClearPersistentVariable(eval, wstring_to_utf8(variable));
}
//=============================================================================
bool
ClearPersistentVariable(Evaluator* eval, const std::string& variable)
{
    bool res = false;
    FunctionDefPtr func = nullptr;

    if (FunctionsInMemory::getInstance()->find(variable, func)) {
        if (func->type() == NLS_MACRO_FUNCTION) {
            auto* macroFunction = static_cast<MacroFunctionDef*>(func);
            res = clearBytecodePersistentVariables(eval, macroFunction) || res;
            const std::string prefix = "_" + variable + "_";
            stringVector allVariableNames;
            eval->getContext()->getGlobalScope()->getVariablesList(true, allVariableNames);
            for (const std::string& name : allVariableNames) {
                if (StringHelpers::starts_with(name, prefix)) {
                    // Delete every matching entry: use a separate flag so the
                    // short-circuit of 'res || deleteVariable(...)' does not stop
                    // deleting the remaining persistent variables of the function.
                    bool deleted = eval->getContext()->getGlobalScope()->deleteVariable(name);
                    res = res || deleted;
                }
            }
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
