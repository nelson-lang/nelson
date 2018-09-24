//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "EvaluateCommand.hpp"
#include "AstManager.hpp"
#include "Error.hpp"
#include "EvaluateInterface.hpp"
#include "ParserInterface.hpp"
#include "characters_encoding.hpp"
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
bool
EvaluateCommand(Evaluator* eval, std::wstring command, bool bCatch)
{
    return EvaluateCommand(eval, wstring_to_utf8(command), bCatch);
}
//=============================================================================
bool
EvaluateCommand(Evaluator* eval, std::string command, bool bCatch)
{
    return eval->evaluateString(command, bCatch);
}
//=============================================================================
#define TEMP_VAR_NAME L"__eval_tmp_"
//=============================================================================
static std::wstring
prepareVariablesReturned(int nLhs, std::wstring command)
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
EvaluateCommand(
    Evaluator* eval, int nLhs, std::wstring command, std::wstring catchCommand, SCOPE_LEVEL scope)
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
            if (retval.size() != nLhs) {
                Error(_W("Invalid use of statement list."));
            }
        }
    } else {
        if (nLhs == 0) {
            bool autostop = eval->AutoStop();
            eval->AutoStop(false);
            try {
                eval->getContext()->bypassScope(scopeValue);
                eval->evaluateString(command, true);
                eval->getContext()->restoreBypassedScopes();
            } catch (const Exception&) {
                eval->getContext()->restoreBypassedScopes();
                eval->evaluateString(catchCommand, false);
            }
            eval->AutoStop(true);
        } else {
            std::wstring preparedCommand = prepareVariablesReturned(nLhs, command);
            std::wstring preparedCatchCommand = prepareVariablesReturned(nLhs, catchCommand);
            bool autostop = eval->AutoStop();
            eval->AutoStop(false);
            try {
                eval->getContext()->bypassScope(scopeValue);
                eval->evaluateString(preparedCommand, true);
                retval = retrieveVariablesReturned(eval, nLhs);
                eval->getContext()->restoreBypassedScopes();
            } catch (const Exception&) {
                eval->getContext()->restoreBypassedScopes();
                eval->evaluateString(preparedCatchCommand, false);
                retval = retrieveVariablesReturned(eval, nLhs);
            }
            eval->AutoStop(true);
            if (retval.size() != nLhs) {
                Error(_W("Invalid use of statement list."));
            }
        }
    }
    return retval;
}
//=============================================================================
ArrayOfVector
EvaluateCommand(Evaluator* eval, int nLhs, std::wstring command, std::wstring catchCommand)
{
    return EvaluateCommand(eval, nLhs, command, catchCommand, SCOPE_LEVEL::LOCAL_SCOPE);
}
//=============================================================================
ArrayOfVector
EvaluateInCommand(Evaluator* eval, int nLhs, SCOPE_LEVEL scope, std::wstring command)
{
    if (scope == GLOBAL_SCOPE) {
        Error(_W("'local', 'caller', 'base' scope expected."));
    }
    return EvaluateCommand(eval, nLhs, command, L"", scope);
}
//=============================================================================
ArrayOfVector
EvaluateConsoleCommand(Evaluator* eval, int nLhs, std::wstring command, std::wstring catchCommand)
{
    ArrayOfVector retval;
    Interface* io = eval->getInterface();
    EvaluateInterface* tempIO = nullptr;
    try {
        tempIO = new EvaluateInterface();
    } catch (const std::bad_alloc&) {
        Error(ERROR_MEMORY_ALLOCATION);
    }
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
        delete tempIO;
    } catch (const Exception&) {
        if (catchCommand != L"") {
            try {
                retval = EvaluateCommand(eval, nbOutput, catchCommand, L"");
                buffer = tempIO->getOutputBuffer();
                eval->setInterface(io);
                delete tempIO;
            } catch (const Exception&) {
                eval->setInterface(io);
                delete tempIO;
                throw;
            }
        } else {
            eval->setInterface(io);
            delete tempIO;
            throw;
        }
    }
    retval.insert(retval.begin(), ArrayOf::characterArrayConstructor(buffer));
    return retval;
}
//=============================================================================
}
//=============================================================================
