//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "LocalFunctions.hpp"
#include "AnonymousMacroFunctionDef.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "NelsonException.hpp"
#include "FileSystemWrapper.hpp"
#include "MacroFunctionDef.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
#include <algorithm>
#include <memory>
#include <new>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    bool
    sameFilename(const std::wstring& left, const std::wstring& right)
    {
        if (left.empty() || right.empty()) {
            return false;
        }
        FileSystemWrapper::Path leftPath(left);
        FileSystemWrapper::Path rightPath(right);
        if (FileSystemWrapper::Path::equivalent(leftPath, rightPath)) {
            return true;
        }
        return leftPath.generic_wstring() == rightPath.generic_wstring();
    }
    //=============================================================================
    Scope*
    currentScope(Evaluator* eval)
    {
        if (eval == nullptr || eval->getContext() == nullptr) {
            return nullptr;
        }
        return eval->getContext()->getCurrentScope();
    }
    //=============================================================================
    Scope*
    callerScope(Evaluator* eval)
    {
        if (eval == nullptr || eval->getContext() == nullptr) {
            return nullptr;
        }
        return eval->getContext()->getCallerScope();
    }
    //=============================================================================
    std::wstring
    scopeFilename(Scope* scope)
    {
        if (scope == nullptr) {
            return L"";
        }
        std::wstring filename = scope->getFilename();
        if (FileSystemWrapper::Path::is_regular_file(filename)) {
            return filename;
        }
        return L"";
    }
    //=============================================================================
    std::wstring
    activeMacroFilename(Evaluator* eval)
    {
        if (eval == nullptr) {
            return L"";
        }
        for (int k = static_cast<int>(eval->callstack.size()) - 2; k >= 0; --k) {
            std::wstring filename = utf8_to_wstring(eval->callstack.getContext(k));
            if (FileSystemWrapper::Path::is_regular_file(filename)) {
                return filename;
            }
        }
        std::wstring filename = scopeFilename(currentScope(eval));
        if (!filename.empty()) {
            return filename;
        }
        filename = scopeFilename(callerScope(eval));
        if (!filename.empty()) {
            return filename;
        }
        return eval->getCurrentEvaluateFilename();
    }
    //=============================================================================
    MacroFunctionDef*
    asMacroFunctionDef(FunctionDefPtr functionDef)
    {
        if (functionDef == nullptr || functionDef->type() != NLS_MACRO_FUNCTION) {
            return nullptr;
        }
        return dynamic_cast<MacroFunctionDef*>(functionDef);
    }
    //=============================================================================
    MacroFunctionDef*
    rootFunction(MacroFunctionDef* functionDef)
    {
        if (functionDef == nullptr) {
            return nullptr;
        }
        std::vector<MacroFunctionDef*> visited;
        while (functionDef->prevFunction != nullptr) {
            if (std::find(visited.begin(), visited.end(), functionDef) != visited.end()) {
                break;
            }
            visited.push_back(functionDef);
            functionDef = functionDef->prevFunction;
        }
        return functionDef;
    }
    //=============================================================================
    MacroFunctionDef*
    findOwningFunctionInScope(Evaluator* eval, Scope* scope, const std::wstring& filename)
    {
        if (eval == nullptr || scope == nullptr || filename.empty()) {
            return nullptr;
        }

        FunctionDefPtr functionDef = nullptr;
        if (scope->lookupFunction(scope->getName(), functionDef)) {
            MacroFunctionDef* macroDef = asMacroFunctionDef(functionDef);
            if (macroDef != nullptr && sameFilename(macroDef->getFilename(), filename)) {
                return rootFunction(macroDef);
            }
        }

        if (eval->lookupFunction(scope->getName(), functionDef)) {
            MacroFunctionDef* macroDef = asMacroFunctionDef(functionDef);
            if (macroDef != nullptr && sameFilename(macroDef->getFilename(), filename)) {
                return rootFunction(macroDef);
            }
        }

        for (const auto& localFunction : scope->getLocalFunctionsList()) {
            MacroFunctionDef* macroDef = asMacroFunctionDef(localFunction.second);
            if (macroDef != nullptr && sameFilename(macroDef->getFilename(), filename)) {
                return rootFunction(macroDef);
            }
        }

        return nullptr;
    }
    //=============================================================================
    MacroFunctionDef*
    findOwningFunction(Evaluator* eval, const std::wstring& filename)
    {
        MacroFunctionDef* owner = findOwningFunctionInScope(eval, currentScope(eval), filename);
        if (owner != nullptr) {
            return owner;
        }
        return findOwningFunctionInScope(eval, callerScope(eval), filename);
    }
    //=============================================================================
    ArrayOf
    emptyCell()
    {
        return ArrayOf(NLS_CELL_ARRAY, Dimensions(0, 0), nullptr);
    }
    //=============================================================================
    bool
    containsFunction(const std::vector<MacroFunctionDef*>& functions, MacroFunctionDef* functionDef)
    {
        return std::find(functions.begin(), functions.end(), functionDef) != functions.end();
    }
    //=============================================================================
    void
    appendLocalFunction(std::vector<MacroFunctionDef*>& functions, MacroFunctionDef* functionDef,
        const std::wstring& filename)
    {
        std::wstring functionFilename = functionDef == nullptr ? L"" : functionDef->getFilename();
        if (functionDef != nullptr && functionDef->localFunction && !functionDef->nestedFunction
            && (functionFilename.empty() || sameFilename(functionFilename, filename))
            && !containsFunction(functions, functionDef)) {
            functions.push_back(functionDef);
        }
    }
    //=============================================================================
    ArrayOf
    localFunctionHandle(MacroFunctionDef* functionDef)
    {
        std::unique_ptr<AnonymousMacroFunctionDef> handleDef;
        try {
            handleDef.reset(
                new AnonymousMacroFunctionDef(functionDef->getName(), functionDef, {}, {}));
        } catch (const std::bad_alloc&) {
            handleDef.reset();
        } catch (const NelsonException&) {
            handleDef.reset();
        }
        if (handleDef == nullptr) {
            Error("Nelson:dispatcher:invalidFunctionName", "A valid function name expected.");
        }
        function_handle handle;
        handle.anonymousHandle = reinterpret_cast<nelson_handle*>(handleDef.get());
        ArrayOf result = ArrayOf::functionHandle(handle);
        handleDef.release();
        return result;
    }
    //=============================================================================
    ArrayOf
    localFunctionsCell(MacroFunctionDef* root, Scope* currentScope, const std::wstring& filename)
    {
        std::vector<MacroFunctionDef*> localFunctions;
        MacroFunctionDef* current = root == nullptr ? nullptr : root->nextFunction;
        std::vector<MacroFunctionDef*> visited;
        while (current != nullptr
            && std::find(visited.begin(), visited.end(), current) == visited.end()) {
            visited.push_back(current);
            appendLocalFunction(localFunctions, current, filename);
            current = current->nextFunction;
        }
        if (root == nullptr || !root->getIsScript()) {
            std::reverse(localFunctions.begin(), localFunctions.end());
        }
        bool orderedFromFunctionChain = !localFunctions.empty();

        if (currentScope != nullptr) {
            for (const auto& localFunction : currentScope->getLocalFunctionsList()) {
                appendLocalFunction(
                    localFunctions, asMacroFunctionDef(localFunction.second), filename);
            }
        }
        if (!orderedFromFunctionChain) {
            std::reverse(localFunctions.begin(), localFunctions.end());
        }
        if (localFunctions.empty()) {
            return emptyCell();
        }

        ArrayOf* elements = static_cast<ArrayOf*>(
            ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, localFunctions.size(), stringVector(), false));
        for (size_t k = 0; k < localFunctions.size(); ++k) {
            elements[k] = localFunctionHandle(localFunctions[k]);
        }
        return ArrayOf(
            NLS_CELL_ARRAY, Dimensions(static_cast<indexType>(localFunctions.size()), 1), elements);
    }
    //=============================================================================
} // namespace
//=============================================================================
ArrayOf
LocalFunctions(Evaluator* eval)
{
    std::wstring filename = activeMacroFilename(eval);
    MacroFunctionDef* owner = findOwningFunction(eval, filename);
    Scope* scope = currentScope(eval);
    ArrayOf result = localFunctionsCell(owner, scope, filename);
    if (!result.isEmpty()) {
        return result;
    }
    Scope* caller = callerScope(eval);
    if (caller != scope) {
        return localFunctionsCell(owner, caller, filename);
    }
    return result;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
