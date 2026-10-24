//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cctype>
#include <unordered_set>
#include <utility>
#include "Error.hpp"
#include "ErrorID.hpp"
#include "NelsonException.hpp"
#include "i18n.hpp"
#include "StringToFunctionHandle.hpp"
#include "characters_encoding.hpp"
#include "IsValidVariableName.hpp"
#include "AnonymousCapturedNames.hpp"
#include "AnonymousMacroFunctionDef.hpp"
#include "MacroFunctionDef.hpp"
#include "StringHelpers.hpp"
#include "ParserInterface.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
isValidNelsonFunctionName(const std::string& functionName);
// Captures the variables of the current scope, restricted to referencedNames
// when it is not null.
static void
getVariablesFromCurrentScope(Evaluator* eval,
    const std::unordered_set<std::string>* referencedNames, stringVector& variableNames,
    std::vector<ArrayOf>& variables);
static std::wstring
getFilenameFromCurrentScope(Evaluator* eval);
static std::vector<std::pair<std::string, FunctionDefPtr>>
getLocalFunctionsFromCurrentScope(Evaluator* eval);
static void
createFunctionHandleNamed(
    Evaluator* eval, const std::string& name, function_handle& functionHandle);
static void
createFunctionHandleAnonymous(const std::string& name, const stringVector& arguments,
    Evaluator* eval, function_handle& functionHandle);
//=============================================================================
function_handle
StringToFunctionHandle(Evaluator* eval, const std::string& functionName)
{
    function_handle functionHandle;
    functionHandle.anonymousHandle = nullptr;

    std::string functionNameReworked = functionName;
    StringHelpers::trim_left(functionNameReworked);
    StringHelpers::trim_right(functionNameReworked);

    ParserState parserState = ParseError;
    NelsonException previousException(eval->getLastErrorException());
    try {
        parserState = parseString(eval->lexerContext, functionNameReworked + "\n");
    } catch (const NelsonException&) {
        parserState = ParseError;
    }
    eval->setLastErrorException(previousException);

    switch (parserState) {
    case ScriptBlock: {
        AbstractSyntaxTreePtr t = getParsedScriptBlock();
        if (t->down && t->down->down) {
            t = t->down->down;
        } else {
            Error("Nelson:dispatcher:invalidFunctionHandle", "A valid function handle expected.");
        }
        if (t->opNum == OP_FUNCTION_HANDLE_NAMED) {
            std::string content;
            if (t->down) {
                content = t->down->text;
            }
            if (content.empty()) {
                Error(
                    "Nelson:dispatcher:invalidFunctionHandle", "A valid function handle expected.");
            }
            createFunctionHandleNamed(eval, content, functionHandle);
        } else if (t->opNum == OP_FUNCTION_HANDLE_ANONYMOUS) {
            AbstractSyntaxTreePtr code = nullptr;
            stringVector arguments;
            if (!t->down) {
                Error(
                    "Nelson:dispatcher:invalidFunctionHandle", "A valid function handle expected.");
            }
            if (t->down->right == nullptr) {
                if (t->down) {
                    code = t->down;
                }
            } else {
                if (t->down) {
                    arguments = t->down->toStringList();
                    code = t->down->right;
                }
            }
            std::string content;
            if (code) {
                content = code->toString(true);
            }
            createFunctionHandleAnonymous(content, arguments, eval, functionHandle);
            reinterpret_cast<AnonymousMacroFunctionDef*>(functionHandle.anonymousHandle)
                ->setDefinitionText(t->text);
        } else {
            AbstractSyntaxTreePtr code = t;
            std::string content;
            if (code) {
                content = code->toString(true);
            }
            if (content.empty()) {
                Error(
                    "Nelson:dispatcher:invalidFunctionHandle", "A valid function handle expected.");
            }
            if (isValidNelsonFunctionName(content)) {
                createFunctionHandleNamed(eval, content, functionHandle);
            } else {
                stringVector arguments;
                createFunctionHandleAnonymous(content, arguments, eval, functionHandle);
            }
        }
    } break;
    case FuncDef:
    case ParseError:
    default: {
        Error("Nelson:dispatcher:invalidFunctionHandle", "A valid function handle expected.");
    } break;
    }
    return functionHandle;
}
//=============================================================================
bool
isValidNelsonFunctionName(const std::string& functionName)
{
    if (!functionName.empty()) {
        size_t start = functionName[0] == '@' ? 1 : 0;
        if (start >= functionName.size()) {
            return false;
        }
        bool expectIdentifierStart = true;
        for (size_t i = start; i < functionName.size(); ++i) {
            char currentChar = functionName[i];
            if (expectIdentifierStart) {
                if (!(isalpha(static_cast<unsigned char>(currentChar)) || currentChar == '_')) {
                    return false;
                }
                expectIdentifierStart = false;
            } else if (currentChar == '.') {
                expectIdentifierStart = true;
            } else if (!(isalnum(static_cast<unsigned char>(currentChar)) || currentChar == '_')) {
                return false;
            }
        }
        return !expectIdentifierStart;
    }
    return false;
}
//=============================================================================
void
getVariablesFromCurrentScope(Evaluator* eval,
    const std::unordered_set<std::string>* referencedNames, stringVector& variableNames,
    std::vector<ArrayOf>& variables)
{
    if (!eval) {
        return;
    }
    Context* context = eval->getContext();
    if (!context) {
        return;
    }
    Scope* scope = context->getCurrentScope();
    if (!scope) {
        return;
    }
    stringVector scopeNames;
    scope->getVariablesList(false, scopeNames);
    variableNames.reserve(scopeNames.size());
    variables.reserve(scopeNames.size());
    for (const std::string& name : scopeNames) {
        if (referencedNames != nullptr && referencedNames->find(name) == referencedNames->end()) {
            continue;
        }
        ArrayOf variable;
        if (scope->lookupVariable(name, variable)) {
            variableNames.push_back(name);
            variables.push_back(variable);
        }
    }
}
//=============================================================================
void
createFunctionHandleNamed(Evaluator* eval, const std::string& name, function_handle& functionHandle)
{
    FunctionDef* localFunctionDef = nullptr;
    if (eval != nullptr && eval->getContext() != nullptr
        && eval->getContext()->getCurrentScope() != nullptr
        && eval->getContext()->getCurrentScope()->lookupFunction(name, localFunctionDef)
        && localFunctionDef != nullptr && localFunctionDef->type() == NLS_MACRO_FUNCTION) {
        auto* macroDef = dynamic_cast<MacroFunctionDef*>(localFunctionDef);
        if (macroDef != nullptr && macroDef->localFunction) {
            stringVector variableNames;
            std::vector<ArrayOf> variables;
            if (macroDef->nestedFunction) {
                getVariablesFromCurrentScope(eval, nullptr, variableNames, variables);
            }
            AnonymousMacroFunctionDef* nestedHandle = nullptr;
            try {
                nestedHandle
                    = new AnonymousMacroFunctionDef(name, macroDef, variableNames, variables);
            } catch (std::bad_alloc&) {
                nestedHandle = nullptr;
            } catch (NelsonException&) {
                delete nestedHandle;
                nestedHandle = nullptr;
            }
            if (nestedHandle) {
                functionHandle.anonymousHandle = reinterpret_cast<nelson_handle*>(nestedHandle);
                return;
            }
            Error("Nelson:dispatcher:invalidFunctionName", "A valid function name expected.");
        }
    }

    AnonymousMacroFunctionDef* cp = nullptr;
    try {
        cp = new AnonymousMacroFunctionDef(name);
    } catch (std::bad_alloc&) {
        cp = nullptr;
    } catch (NelsonException&) {
        delete cp;
        cp = nullptr;
    }
    if (cp) {
        functionHandle.anonymousHandle = reinterpret_cast<nelson_handle*>(cp);
    } else {
        Error("Nelson:dispatcher:invalidFunctionName", "A valid function name expected.");
    }
}
//=============================================================================
void
createFunctionHandleAnonymous(const std::string& content, const stringVector& arguments,
    Evaluator* eval, function_handle& functionHandle)
{
    stringVector variableNames;
    std::vector<ArrayOf> variables;
    const std::unordered_set<std::string> referenced = anonymousReferencedNames(content, arguments);
    std::vector<std::pair<std::string, FunctionDefPtr>> localFunctions
        = getLocalFunctionsFromCurrentScope(eval);
    // a body calling a nested function needs the whole parent workspace
    const bool captureAll = anonymousReferencesNestedFunction(referenced, localFunctions);
    getVariablesFromCurrentScope(
        eval, captureAll ? nullptr : &referenced, variableNames, variables);
    std::wstring sourceFilename;
    if (anonymousContentMayNeedFunctionScope(referenced, variableNames)) {
        sourceFilename = getFilenameFromCurrentScope(eval);
    } else {
        localFunctions.clear();
    }

    AnonymousMacroFunctionDef* cp = nullptr;
    try {
        cp = new AnonymousMacroFunctionDef(
            content, arguments, variableNames, variables, sourceFilename, localFunctions);
    } catch (std::bad_alloc&) {
        cp = nullptr;
    } catch (NelsonException&) {
        delete cp;
        cp = nullptr;
    }
    if (cp) {
        functionHandle.anonymousHandle = reinterpret_cast<nelson_handle*>(cp);
    } else {
        Error("Nelson:dispatcher:invalidFunctionName", "A valid function name expected.");
    }
}
//=============================================================================
std::wstring
getFilenameFromCurrentScope(Evaluator* eval)
{
    if (!eval || !eval->getContext() || !eval->getContext()->getCurrentScope()) {
        return L"";
    }
    return eval->getContext()->getCurrentScope()->getFilename();
}
//=============================================================================
std::vector<std::pair<std::string, FunctionDefPtr>>
getLocalFunctionsFromCurrentScope(Evaluator* eval)
{
    if (!eval || !eval->getContext() || !eval->getContext()->getCurrentScope()) {
        return {};
    }
    return eval->getContext()->getCurrentScope()->getLocalFunctionsList();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
