//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Error.hpp"
#include "Exception.hpp"
#include "i18n.hpp"
#include "StringToFunctionHandle.hpp"
#include "characters_encoding.hpp"
#include "IsValidVariableName.hpp"
#include "AnonymousMacroFunctionDef.hpp"
#include "StringHelpers.hpp"
#include "ParserInterface.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
isValidNelsonFunctionName(const std::string& functionName);
static void
getVariablesFromCurrentScope(
    Evaluator* eval, stringVector& variableNames, std::vector<ArrayOf>& variables);
static void
createFunctionHandleNamed(const std::string& name, function_handle& functionHandle);
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
    Exception previousException(eval->getLastErrorException());
    try {
        parserState = parseString(eval->lexerContext, functionNameReworked + "\n");
    } catch (const Exception&) {
        parserState = ParseError;
    }
    eval->setLastErrorException(previousException);

    switch (parserState) {
    case ScriptBlock: {
        AbstractSyntaxTreePtr t = getParsedScriptBlock();
        if (t->down && t->down->down) {
            t = t->down->down;
        } else {
            Error(_("A valid function handle expected."), "Nelson:dispatcher:invalidFunctionName");
        }
        if (t->opNum == OP_FUNCTION_HANDLE_NAMED) {
            std::string content;
            if (t->down) {
                content = t->down->text;
            }
            if (content.empty()) {
                Error(_("A valid function handle expected."),
                    "Nelson:dispatcher:invalidFunctionName");
            }
            createFunctionHandleNamed(content, functionHandle);
        } else if (t->opNum == OP_FUNCTION_HANDLE_ANONYMOUS) {
            AbstractSyntaxTreePtr code = nullptr;
            stringVector arguments;
            if (!t->down) {
                Error(_("A valid function handle expected."),
                    "Nelson:dispatcher:invalidFunctionName");
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
        } else {
            AbstractSyntaxTreePtr code = t;
            std::string content;
            if (code) {
                content = code->toString(true);
            }
            if (content.empty()) {
                Error(_("A valid function handle expected."),
                    "Nelson:dispatcher:invalidFunctionName");
            }
            if (isValidNelsonFunctionName(content)) {
                createFunctionHandleNamed(content, functionHandle);
            } else {
                stringVector arguments;
                createFunctionHandleAnonymous(content, arguments, eval, functionHandle);
            }
        }
    } break;
    case FuncDef:
    case ParseError:
    default: {
        Error(_("A valid function handle expected."), "Nelson:dispatcher:invalidFunctionName");
    } break;
    }
    return functionHandle;
}
//=============================================================================
bool
isValidNelsonFunctionName(const std::string& functionName)
{
    if (functionName.size() > 0) {
        size_t pos = 0;
        if (functionName[0] == '@') {
            pos = 1;
        }
        if (!(isalpha(functionName[pos]) || functionName[pos] == '_')) {
            return false;
        }
        for (size_t i = pos; i < functionName.size(); ++i) {
            char currentChar = functionName[i];
            if (!(isalnum(currentChar) || currentChar == '_')) {
                return false;
            }
        }
        return true;
    }
    return false;
}
//=============================================================================
void
getVariablesFromCurrentScope(
    Evaluator* eval, stringVector& variableNames, std::vector<ArrayOf>& variables)
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
    scope->getVariablesList(false, variableNames);
    variables.reserve(variableNames.size());
    for (auto name : variableNames) {
        ArrayOf variable;
        if (scope->lookupVariable(name, variable)) {
            variables.push_back(variable);
        }
    }
}
//=============================================================================
void
createFunctionHandleNamed(const std::string& name, function_handle& functionHandle)
{
    AnonymousMacroFunctionDef* cp = nullptr;
    try {
        cp = new AnonymousMacroFunctionDef(name);
    } catch (std::bad_alloc&) {
        cp = nullptr;
    } catch (Exception&) {
        delete cp;
        cp = nullptr;
    }
    if (cp) {
        functionHandle.anonymousHandle = reinterpret_cast<nelson_handle*>(cp);
    } else {
        Error(_("A valid function name expected."), "Nelson:dispatcher:invalidFunctionName");
    }
}
//=============================================================================
void
createFunctionHandleAnonymous(const std::string& content, const stringVector& arguments,
    Evaluator* eval, function_handle& functionHandle)
{
    stringVector variableNames;
    std::vector<ArrayOf> variables;
    getVariablesFromCurrentScope(eval, variableNames, variables);

    AnonymousMacroFunctionDef* cp = nullptr;
    try {
        cp = new AnonymousMacroFunctionDef(content, arguments, variableNames, variables);
    } catch (std::bad_alloc&) {
        cp = nullptr;
    } catch (Exception&) {
        delete cp;
        cp = nullptr;
    }
    if (cp) {
        functionHandle.anonymousHandle = reinterpret_cast<nelson_handle*>(cp);
    } else {
        Error(_("A valid function name expected."), "Nelson:dispatcher:invalidFunctionName");
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
