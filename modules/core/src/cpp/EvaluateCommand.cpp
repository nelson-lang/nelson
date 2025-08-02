//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <string>
#include "EvaluateCommand.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "EvaluateInterface.hpp"
#include "ParserInterface.hpp"
#include "characters_encoding.hpp"
#include "NelsonPrint.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
EvaluateCommand(Evaluator* eval, const std::wstring& command, bool bCatch)
{
    return EvaluateCommand(eval, wstring_to_utf8(command), bCatch);
}
//=============================================================================
bool
EvaluateCommand(Evaluator* eval, const std::string& command, bool bCatch)
{
    return eval->evaluateString(command, bCatch);
}
//=============================================================================
#define TEMP_VAR_NAME L"__eval_tmp_"
//=============================================================================
static std::wstring
prepareVariablesReturned(int nLhs, const std::wstring& command)
{
    std::wstring variables;
    if (nLhs > 1) {
        variables.append(L"[");
    }
    for (int v = 0; v < nLhs - 1; v++) {
        variables.append(std::wstring(TEMP_VAR_NAME) + std::to_wstring(v) + L", ");
    }
    variables.append(std::wstring(TEMP_VAR_NAME) + std::to_wstring(nLhs - 1));
    if (nLhs > 1) {
        variables.append(L"] = ");
        variables.append(command);
    } else {
        variables.append(L" = ");
        variables.append(command);
    }
    variables.append(L";");
    return variables;
}
//=============================================================================
static ArrayOfVector
retrieveVariablesReturned(Evaluator* eval, int nLhs)
{
    ArrayOfVector variables;
    for (int v = 0; v < nLhs; v++) {
        std::wstring varname = std::wstring(TEMP_VAR_NAME) + std::to_wstring(v);
        ArrayOf var;
        if (!eval->getContext()->lookupVariableLocally(wstring_to_utf8(varname), var)) {
            var = ArrayOf::emptyConstructor();
        } else {
            var.name("");
            variables.push_back(var);
        }
        eval->getContext()->deleteVariable(wstring_to_utf8(varname));
    }
    return variables;
}
//=============================================================================
static int
getScopeValue(SCOPE_LEVEL scope)
{
    switch (scope) {
    case SCOPE_LEVEL::BASE_SCOPE:
        return -1;
    case SCOPE_LEVEL::CALLER_SCOPE:
        return 1;
    case SCOPE_LEVEL::LOCAL_SCOPE:
    default:
        return 0;
    }
}
//=============================================================================
static ArrayOfVector
EvaluateCommand(Evaluator* eval, int nLhs, const std::wstring& command,
    const std::wstring& catchCommand, SCOPE_LEVEL scope)
{
    int scopeValue = getScopeValue(scope);
    ArrayOfVector retval;
    if (catchCommand.empty()) {
        if (nLhs == 0) {
            eval->getContext()->bypassScope(scopeValue);
            eval->evaluateString(command);
            eval->getContext()->restoreBypassedScopes();
        } else {
            std::wstring preparedCommand = prepareVariablesReturned(nLhs, command);
            eval->getContext()->bypassScope(scopeValue);
            eval->evaluateString(preparedCommand);
            retval = retrieveVariablesReturned(eval, nLhs);
            eval->getContext()->restoreBypassedScopes();
            if (retval.size() != static_cast<size_t>(nLhs)) {
                Error(_W("Invalid use of statement list."));
            }
        }
    } else {
        if (nLhs == 0) {
            bool autostop = eval->AutoStop();
            eval->AutoStop(false);
            Context* context = eval->getContext();
            try {
                context->bypassScope(scopeValue);
                eval->evaluateString(command, true);
                context->restoreBypassedScopes();
            } catch (const Exception&) {
                context->restoreBypassedScopes();
                eval->evaluateString(catchCommand, false);
            }
            eval->AutoStop(true);
        } else {
            std::wstring preparedCommand = prepareVariablesReturned(nLhs, command);
            std::wstring preparedCatchCommand = prepareVariablesReturned(nLhs, catchCommand);
            bool autostop = eval->AutoStop();
            eval->AutoStop(false);
            Context* context = eval->getContext();
            try {
                context->bypassScope(scopeValue);
                eval->evaluateString(preparedCommand, true);
                retval = retrieveVariablesReturned(eval, nLhs);
                context->restoreBypassedScopes();
            } catch (const Exception&) {
                context->restoreBypassedScopes();
                eval->evaluateString(preparedCatchCommand, false);
                retval = retrieveVariablesReturned(eval, nLhs);
            }
            eval->AutoStop(true);
            if (retval.size() != static_cast<size_t>(nLhs)) {
                Error(_W("Invalid use of statement list."));
            }
        }
    }
    return retval;
}
//=============================================================================
ArrayOfVector
EvaluateCommand(
    Evaluator* eval, int nLhs, const std::wstring& command, const std::wstring& catchCommand)
{
    return EvaluateCommand(eval, nLhs, command, catchCommand, SCOPE_LEVEL::LOCAL_SCOPE);
}
//=============================================================================
ArrayOfVector
EvaluateInCommand(Evaluator* eval, int nLhs, SCOPE_LEVEL scope, const std::wstring& command)
{
    if (scope == GLOBAL_SCOPE) {
        Error(_W("'local', 'caller', 'base' scope expected."));
    }
    return EvaluateCommand(eval, nLhs, command, L"", scope);
}
//=============================================================================
ArrayOfVector
EvaluateConsoleCommand(
    Evaluator* eval, int nLhs, const std::wstring& command, const std::wstring& catchCommand)
{
    ArrayOfVector retval;
    Interface* io = eval->getInterface();
    EvaluateInterface* tempIO = nullptr;
    try {
        tempIO = new EvaluateInterface();
    } catch (const std::bad_alloc&) {
        Error(ERROR_MEMORY_ALLOCATION);
    }
    setPrintInterface(tempIO);
    eval->setInterface(tempIO);
    eval->getContext()->bypassScope(0);
    std::wstring buffer;
    int nbOutput;
    if (nLhs == 0) {
        nbOutput = 0;
    } else {
        nbOutput = nLhs - 1;
    }
    try {
        retval = EvaluateCommand(eval, nbOutput, command, L"");
        buffer = tempIO->getOutputBuffer();
        eval->setInterface(io);
        setPrintInterface(io);
        delete tempIO;
    } catch (const Exception&) {
        if (!catchCommand.empty()) {
            try {
                retval = EvaluateCommand(eval, nbOutput, catchCommand, L"");
                buffer = tempIO->getOutputBuffer();
                eval->setInterface(io);
                setPrintInterface(io);
                delete tempIO;
            } catch (const Exception&) {
                eval->setInterface(io);
                setPrintInterface(io);
                delete tempIO;
                throw;
            }
        } else {
            eval->setInterface(io);
            setPrintInterface(io);
            delete tempIO;
            throw;
        }
    }
    retval.push_front(ArrayOf::characterArrayConstructor(buffer));
    return retval;
}
//=============================================================================
bool
EvaluateConsoleCommandToString(Evaluator* eval, const std::wstring& command, std::wstring& result)
{
    bool success = false;
    result.clear();

    Interface* io = eval->getInterface();
    EvaluateInterface* tempIO = nullptr;
    try {
        tempIO = new EvaluateInterface();
    } catch (const std::bad_alloc&) {
        result = _W("Error:") + _W(ERROR_MEMORY_ALLOCATION);
        return success;
    }
    setPrintInterface(tempIO);
    eval->setInterface(tempIO);
    eval->getContext()->bypassScope(0);
    try {
        EvaluateCommand(eval, 0, command, L"", SCOPE_LEVEL::BASE_SCOPE);
        result = tempIO->getOutputBuffer();
        eval->setInterface(io);
        setPrintInterface(io);
        delete tempIO;
        success = true;
    } catch (Exception& e) {
        e.printMe(tempIO);
        result = tempIO->getOutputBuffer();
        eval->setInterface(io);
        setPrintInterface(io);
        delete tempIO;
        success = false;
    }
    return success;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
