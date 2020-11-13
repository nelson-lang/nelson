//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "ipcBuiltin.hpp"
#include "Error.hpp"
#include "NelsonInterprocess.hpp"
#include "IsValidVariableName.hpp"
#include "NelsonPIDs.hpp"
#include "NelsonInterprocess.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static NELSON_INTERPROCESS_COMMAND
commandTypeToValueSwitch(const std::wstring& command)
{
    if (command == L"eval") {
        return NELSON_INTERPROCESS_COMMAND::EVAL;
    }
    if (command == L"put") {
        return NELSON_INTERPROCESS_COMMAND::PUT;
    }
    if (command == L"get") {
        return NELSON_INTERPROCESS_COMMAND::GET;
    }
    if (command == L"isvar") {
        return NELSON_INTERPROCESS_COMMAND::IS_VAR;
    }
    if (command == L"minimize") {
        return NELSON_INTERPROCESS_COMMAND::SET_MINIMIZE;
    }
    return NELSON_INTERPROCESS_COMMAND::UNKNOWN;
}
//=============================================================================
static ArrayOfVector
ipcBuiltinThreeRhs(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    // ipc(pid, 'eval', cmd)
    // V = ipc(pid, 'get', name)
    // B = ipc(pid, 'isvar', name)
    // ipc(pid, 'minimize', TF)
    ArrayOfVector retval;
    ArrayOf param1 = argIn[0];
    ArrayOf param2 = argIn[1];
    ArrayOf param3 = argIn[2];
    int32 pid = param1.getContentAsInteger32Scalar();
    if (!isPIDRunning(pid)) {
        Error(_W("PID valid expected."));
    }
    std::wstring commandType = param2.getContentAsWideString();
    switch (commandTypeToValueSwitch(commandType)) {
    case NELSON_INTERPROCESS_COMMAND::EVAL: {
        if (nLhs != 0) {
            Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
        }
        std::wstring command = param3.getContentAsWideString();
        std::wstring errorMessage;
        bool r = sendCommandToNelsonInterprocessReceiver(
            pid, command, eval->haveEventsLoop(), errorMessage);
        if (!r) {
            Error(_W("Command not sent.") + errorMessage);
        }
    } break;
    case NELSON_INTERPROCESS_COMMAND::SET_MINIMIZE: {
        if (nLhs != 0) {
            Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
        }
        bool minimize = param3.getContentAsLogicalScalar() != 0u;
        std::wstring errorMessage;
        bool r = sendMinimizeToNelsonInterprocessReceiver(
            pid, minimize, eval->haveEventsLoop(), errorMessage);
        if (!r) {
            Error(_W("Command not sent.") + errorMessage);
        }
    } break;
    case NELSON_INTERPROCESS_COMMAND::GET: {
        if (nLhs > 1) {
            Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
        }
        std::wstring name = param3.getContentAsWideString();
        if (!IsValidVariableName(name)) {
            Error(_W("#3 Argument must contain a valid variable name."));
        }
        std::wstring errorMessage;
        ArrayOf result = getVariableFromNelsonInterprocessReceiver(
            pid, name, L"local", eval->haveEventsLoop(), errorMessage);
        if (!errorMessage.empty()) {
            Error(errorMessage);
        }
        retval.push_back(result);
    } break;
    case NELSON_INTERPROCESS_COMMAND::IS_VAR: {
        if (nLhs > 1) {
            Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
        }
        std::wstring name = param3.getContentAsWideString();
        if (!IsValidVariableName(name)) {
            Error(_W("#3 Argument must contain a valid variable name."));
        }
        std::wstring errorMessage;
        bool result = isVariableFromNelsonInterprocessReceiver(
            pid, name, L"local", eval->haveEventsLoop(), errorMessage);
        if (!errorMessage.empty()) {
            Error(errorMessage);
        }
        retval.push_back(ArrayOf::logicalConstructor(result));
    } break;
    default: {
        Error(_W("#2 parameter invalid: 'eval', 'put', 'get', 'minimize' or 'isvar' parameter "
                 "expected."));
    } break;
    }
    return retval;
}
//=============================================================================
static ArrayOfVector
ipcBuiltinFourRhs(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    // ipc(pid, 'put', var, name)
    // V = ipc(pid, 'get', name, scope)
    // B = ipc(pid, 'isvar', name, scope)
    ArrayOfVector retval;
    int32 pid;
    ArrayOf param1 = argIn[0];
    ArrayOf param2 = argIn[1];
    ArrayOf param3 = argIn[2];
    ArrayOf param4 = argIn[3];
    pid = param1.getContentAsInteger32Scalar();
    if (!isPIDRunning(pid)) {
        Error(_W("PID valid expected."));
    }
    std::wstring commandType = param2.getContentAsWideString();
    switch (commandTypeToValueSwitch(commandType)) {
    case NELSON_INTERPROCESS_COMMAND::PUT: {
        if (nLhs != 0) {
            Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
        }
        std::wstring name = param4.getContentAsWideString();
        if (!IsValidVariableName(name)) {
            Error(_W("#4 Argument must contain a valid variable name."));
        }
        std::wstring errorMessage;
        bool r = sendVariableToNelsonInterprocessReceiver(
            pid, param3, name, L"local", eval->haveEventsLoop(), errorMessage);
        if (!r) {
            Error(_W("Variable not sent.") + errorMessage);
        }
    } break;
    case NELSON_INTERPROCESS_COMMAND::GET: {
        if (nLhs > 1) {
            Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
        }
        std::wstring name = param3.getContentAsWideString();
        if (!IsValidVariableName(name)) {
            Error(_W("#3 Argument must contain a valid variable name."));
        }
        std::wstring scope = param4.getContentAsWideString();
        bool supported = ((scope == L"global") || (scope == L"base") || (scope == L"caller")
            || (scope == L"local"));
        if (!supported) {
            Error(_W("#4 Argument must contain a valid scope name."));
        }
        std::wstring errorMessage;
        ArrayOf result = getVariableFromNelsonInterprocessReceiver(
            pid, name, scope, eval->haveEventsLoop(), errorMessage);
        if (!errorMessage.empty()) {
            Error(errorMessage);
        }
        retval.push_back(result);
    } break;
    case NELSON_INTERPROCESS_COMMAND::IS_VAR: {
        if (nLhs > 1) {
            Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
        }
        std::wstring name = param3.getContentAsWideString();
        if (!IsValidVariableName(name)) {
            Error(_W("#3 Argument must contain a valid variable name."));
        }
        std::wstring scope = param4.getContentAsWideString();
        bool supported = ((scope == L"global") || (scope == L"base") || (scope == L"caller")
            || (scope == L"local"));
        if (!supported) {
            Error(_W("#4 Argument must contain a valid scope name."));
        }
        std::wstring errorMessage;
        bool result = isVariableFromNelsonInterprocessReceiver(
            pid, name, scope, eval->haveEventsLoop(), errorMessage);
        if (!errorMessage.empty()) {
            Error(errorMessage);
        }
        retval.push_back(ArrayOf::logicalConstructor(result));
    } break;
    default: {
        Error(_W("#2 parameter invalid: 'put', 'get', or 'isvar' parameter expected."));
    } break;
    }
    return retval;
}
//=============================================================================
static ArrayOfVector
ipcBuiltinFiveRhs(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    // ipc(pid, 'put', var, name , scope)
    ArrayOfVector retval;
    ArrayOf param1 = argIn[0]; // pid
    ArrayOf param2 = argIn[1]; // put
    ArrayOf param3 = argIn[2]; // var
    ArrayOf param4 = argIn[3]; // name
    ArrayOf param5 = argIn[4]; // scope
    int32 pid = param1.getContentAsInteger32Scalar();
    if (!isPIDRunning(pid)) {
        Error(_W("PID valid expected."));
    }
    std::wstring commandType = param2.getContentAsWideString();
    switch (commandTypeToValueSwitch(commandType)) {
    case NELSON_INTERPROCESS_COMMAND::PUT: {
        std::wstring name = param4.getContentAsWideString();
        if (!IsValidVariableName(name)) {
            Error(_W("#4 Argument must contain a valid variable name."));
        }
        std::wstring scope = param5.getContentAsWideString();
        bool supported = ((scope == L"global") || (scope == L"base") || (scope == L"caller")
            || (scope == L"local"));
        if (!supported) {
            Error(_W("#4 Argument must contain a valid scope name."));
        }
        if (nLhs != 0) {
            Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
        }
        std::wstring errorMessage;
        bool r = sendVariableToNelsonInterprocessReceiver(
            pid, param3, name, scope, eval->haveEventsLoop(), errorMessage);
        if (!r) {
            Error(_W("Variable not sent.") + errorMessage);
        }
        return retval;
    } break;
    default: {
        Error(_W("#2 parameter invalid: 'put' parameter expected."));
    } break;
    }
    return retval;
}
//=============================================================================
// ipc(pid, 'eval', cmd)
// ipc(pid, 'put', var, name [, scope = current])
// V = ipc(pid, 'get', name [, scope = current])
// B = ipc(pid, 'isvar', name [, scope = current])
// V = ipc(pid, 'minimize')
// ipc(pid, 'minimize', TF)
ArrayOfVector
Nelson::IpcGateway::ipcBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    switch (argIn.size()) {
    case 3: {
        return ipcBuiltinThreeRhs(eval, nLhs, argIn);
    } break;
    case 4: {
        return ipcBuiltinFourRhs(eval, nLhs, argIn);
    } break;
    case 5: {
        return ipcBuiltinFiveRhs(eval, nLhs, argIn);
    } break;
    default: {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    } break;
    }
    return retval;
}
//=============================================================================
