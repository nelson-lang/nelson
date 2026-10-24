//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Function-handle creation and invocation helpers for the bytecode VM.
//=============================================================================
#include <algorithm>
#include <cctype>
#include <string>
#include <unordered_set>
#include <vector>
#include "BytecodeVMInternal.hpp"
#include "AnonymousCapturedNames.hpp"
#include "AnonymousMacroFunctionDef.hpp"
#include "ClassdefParser.hpp"
#include "Context.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "Evaluator.hpp"
#include "FunctionDef.hpp"
#include "MacroFunctionDef.hpp"
#include "NelsonException.hpp"
#include "Scope.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson::bcvm_detail {
//=============================================================================
ArrayOfVector
callFunctionHandle(Evaluator* eval, const ArrayOf& handle, const ArrayOfVector& args, int nLhs)
{
    if (!handle.isFunctionHandle()) {
        // Indexing a class/handle object with "()" (e.g. [a, b] = obj(x))
        // dispatches to the object's subsref with nLhs outputs.
        if (handle.isClassType() || handle.isHandle()) {
            stringVector subtypes;
            subtypes.push_back("()");
            ArrayOfVector subsindices;
            subsindices.push_back(classIndexCell(args));
            bool haveFunction = false;
            int nout = nLhs > 1 ? nLhs : 1;
            ArrayOfVector rv
                = eval->bytecodeExtractClass(handle, subtypes, subsindices, haveFunction, nout);
            if (haveFunction) {
                return rv;
            }
            if (handle.isClassType() && classdefLoadClass(handle.classType())) {
                if (nLhs > 1) {
                    Error(L"Nelson:interpreter:insufficientOutputs",
                        L"Insufficient number of outputs from right hand side of equal sign to "
                        L"satisfy assignment.");
                }
                if (args.empty()) {
                    Error(L"Nelson:interpreter:indexExpressionExpected",
                        L"Index expression expected.");
                }
                ArrayOf obj(handle);
                ArrayOfVector idx(args);
                ArrayOfVector out;
                if (idx.size() == 1) {
                    out.push_back(obj.vectorSubset(idx[0]));
                } else {
                    out.push_back(obj.nDimSubset(idx));
                }
                return out;
            }
            Error(L"Nelson:interpreter:invalidIndexing", L"Invalid indexing.");
        }
        Error(L"Nelson:types:functionHandleExpected",
            L"Argument #1 must be a valid function_handle.");
    }
    function_handle fh = handle.asFunctionHandle();
    FunctionDef* funcDef = reinterpret_cast<FunctionDef*>(fh.anonymousHandle);
    if (funcDef == nullptr) {
        Error(L"Nelson:interpreter:functionDoesNotExist", L"Function does not exist.");
    }
    if (funcDef->type() == NLS_ANONYMOUS_MACRO_FUNCTION) {
        auto* anonymousDef = reinterpret_cast<AnonymousMacroFunctionDef*>(funcDef);
        if (anonymousDef->isStatelessSimpleIdentity() && args.size() == 1 && nLhs <= 1) {
            if (nLhs == 0) {
                return {};
            }
            ArrayOfVector result;
            result.push_back(args[0]);
            return result;
        }
    }
    return funcDef->evaluateFunction(eval, args, nLhs);
}
//=============================================================================
stringVector
splitAnonymousArguments(const std::string& payload)
{
    stringVector values;
    std::string current;
    for (char ch : payload) {
        if (ch == '\x1f') {
            values.push_back(current);
            current.clear();
        } else {
            current.push_back(ch);
        }
    }
    if (!current.empty() || !payload.empty()) {
        values.push_back(current);
    }
    return values;
}
//=============================================================================
ArrayOf
makeAnonymousHandle(Evaluator* eval, VMCallFrame& frame, const std::string& content,
    const stringVector& arguments, const std::string& definitionText)
{
    stringVector variableNames;
    std::vector<ArrayOf> variables;
    // Only the variables the body refers to are captured: the handle must
    // not keep the rest of the workspace alive. A body calling a nested
    // function is the exception: that function reads its parent's
    // workspace, which is then captured whole.
    const std::unordered_set<std::string> referenced = anonymousReferencedNames(content, arguments);
    Scope* scope = eval->getContext()->getCurrentScope();
    std::vector<std::pair<std::string, FunctionDefPtr>> localFunctions;
    if (scope != nullptr) {
        localFunctions = scope->getLocalFunctionsList();
    }
    const bool captureAll = anonymousReferencesNestedFunction(referenced, localFunctions);

    auto addCapture = [&](const std::string& name, const ArrayOf& value) {
        if (name.empty() || (!captureAll && referenced.find(name) == referenced.end())
            || std::find(variableNames.begin(), variableNames.end(), name) != variableNames.end()) {
            return;
        }
        variableNames.push_back(name);
        variables.push_back(value);
    };

    size_t localCount = std::min(frame.chunk->localNames.size(), frame.locals.size());
    for (size_t k = 0; k < localCount; ++k) {
        // Capture only slots that already hold a value when the handle is
        // created. In a script, a slot assigned textually after this anonymous
        // function counts as compile-time assigned but is still empty; capturing
        // it would shadow a package or function of the same name at call time.
        if (!isRuntimeAssignedLocalSlot(frame, k)) {
            continue;
        }
        addCapture(frame.chunk->localNames[k], frame.locals[k]);
    }

    std::wstring sourceFilename;
    if (scope != nullptr) {
        stringVector scopeNames;
        scope->getVariablesList(false, scopeNames);
        for (const std::string& name : scopeNames) {
            if (!captureAll && referenced.find(name) == referenced.end()) {
                continue;
            }
            ArrayOf value;
            if (scope->lookupVariable(name, value)) {
                addCapture(name, value);
            }
        }
        if (anonymousContentMayNeedFunctionScope(referenced, variableNames)) {
            sourceFilename = scope->getFilename();
        } else {
            localFunctions.clear();
        }
    }

    AnonymousMacroFunctionDef* functionDef = nullptr;
    try {
        functionDef = new AnonymousMacroFunctionDef(
            content, arguments, variableNames, variables, sourceFilename, localFunctions);
    } catch (std::bad_alloc&) {
        functionDef = nullptr;
    } catch (const NelsonException&) {
        delete functionDef;
        functionDef = nullptr;
    }
    if (functionDef == nullptr) {
        Error("Nelson:dispatcher:invalidFunctionName", "A valid function name expected.");
    }
    functionDef->setDefinitionText(definitionText);
    function_handle handle;
    handle.anonymousHandle = reinterpret_cast<nelson_handle*>(functionDef);
    return ArrayOf::functionHandle(handle);
}
//=============================================================================
void
collectCurrentFrameAndScopeVariables(Evaluator* eval, VMCallFrame& frame,
    stringVector& variableNames, std::vector<ArrayOf>& variables)
{
    auto addCapture = [&](const std::string& name, const ArrayOf& value) {
        if (name.empty()
            || std::find(variableNames.begin(), variableNames.end(), name) != variableNames.end()) {
            return;
        }
        variableNames.push_back(name);
        variables.push_back(value);
    };

    size_t localCount = std::min(frame.chunk->localNames.size(), frame.locals.size());
    for (size_t k = 0; k < localCount; ++k) {
        if (frame.chunk->localNames[k].empty()) {
            continue;
        }
        addCapture(frame.chunk->localNames[k], frame.locals[k]);
    }

    Scope* scope = eval->getContext()->getCurrentScope();
    if (scope != nullptr) {
        stringVector scopeNames;
        scope->getVariablesList(false, scopeNames);
        for (const std::string& name : scopeNames) {
            ArrayOf value;
            if (scope->lookupVariable(name, value)) {
                addCapture(name, value);
            }
        }
    }
}
//=============================================================================
void
syncNestedFunctionHandlesInFrame(const BytecodeChunk& chunk, VMCallFrame& frame)
{
    if (!chunk.mayCreateNamedFunctionHandle) {
        return;
    }
    std::vector<AnonymousMacroFunctionDef*> nestedHandles;
    const size_t localCount = std::min(frame.locals.size(), chunk.localNames.size());
    nestedHandles.reserve(2);
    for (size_t k = 0; k < localCount; ++k) {
        if (frame.locals[k].dataClass() != NLS_FUNCTION_HANDLE) {
            continue;
        }
        function_handle fh = frame.locals[k].asFunctionHandle();
        if (fh.anonymousHandle == nullptr) {
            continue;
        }
        auto* functionDef = reinterpret_cast<AnonymousMacroFunctionDef*>(fh.anonymousHandle);
        if (functionDef != nullptr && functionDef->isBoundNestedFunctionHandle()) {
            nestedHandles.push_back(functionDef);
        }
    }
    if (nestedHandles.empty()) {
        return;
    }

    stringVector names;
    std::vector<ArrayOf> values;
    names.reserve(localCount);
    values.reserve(localCount);
    for (size_t k = 0; k < localCount; ++k) {
        if (chunk.localNames[k].empty()) {
            continue;
        }
        names.push_back(chunk.localNames[k]);
        values.push_back(frame.locals[k]);
    }

    for (auto* functionDef : nestedHandles) {
        functionDef->syncCapturedVariables(names, values);
    }
}
//=============================================================================
ArrayOf
makeNamedHandle(Evaluator* eval, VMCallFrame& frame, const std::string& name)
{
    FunctionDef* localFunctionDef = nullptr;
    Scope* currentScope = eval->getContext()->getCurrentScope();
    if (currentScope != nullptr && currentScope->lookupFunction(name, localFunctionDef)
        && localFunctionDef != nullptr && localFunctionDef->type() == NLS_MACRO_FUNCTION) {
        auto* macroDef = dynamic_cast<MacroFunctionDef*>(localFunctionDef);
        if (macroDef != nullptr && macroDef->localFunction) {
            stringVector variableNames;
            std::vector<ArrayOf> variables;
            if (macroDef->nestedFunction) {
                collectCurrentFrameAndScopeVariables(eval, frame, variableNames, variables);
            }
            AnonymousMacroFunctionDef* functionDef = nullptr;
            try {
                functionDef
                    = new AnonymousMacroFunctionDef(name, macroDef, variableNames, variables);
            } catch (std::bad_alloc&) {
                functionDef = nullptr;
            } catch (const NelsonException&) {
                delete functionDef;
                functionDef = nullptr;
            }
            if (functionDef == nullptr) {
                Error("Nelson:dispatcher:invalidFunctionName", "A valid function name expected.");
            }
            function_handle handle;
            handle.anonymousHandle = reinterpret_cast<nelson_handle*>(functionDef);
            return ArrayOf::functionHandle(handle);
        }
    }

    AnonymousMacroFunctionDef* functionDef = nullptr;
    try {
        functionDef = new AnonymousMacroFunctionDef(name);
    } catch (std::bad_alloc&) {
        functionDef = nullptr;
    } catch (const NelsonException&) {
        delete functionDef;
        functionDef = nullptr;
    }
    if (functionDef == nullptr) {
        Error("Nelson:dispatcher:invalidFunctionName", "A valid function name expected.");
    }
    function_handle handle;
    handle.anonymousHandle = reinterpret_cast<nelson_handle*>(functionDef);
    return ArrayOf::functionHandle(handle);
}
//=============================================================================
} // namespace Nelson::bcvm_detail
//=============================================================================
