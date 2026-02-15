//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ipcBuiltin.hpp"
#include "PredefinedErrorMessages.hpp"
#include "i18n.hpp"
#include "NelsonInterprocess.hpp"
#include "IsValidVariableName.hpp"
#include "NelsonPIDs.hpp"
#include "EvaluateCommand.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static NELSON_INTERPROCESS_COMMAND
commandTypeToValueSwitch(const std::wstring& command)
{
    if (command == L"post") {
        return NELSON_INTERPROCESS_COMMAND::POST_COMMAND;
    }
    if (command == L"eval") {
        return NELSON_INTERPROCESS_COMMAND::EVAL;
    }
    if (command == L"eval_answer") {
        return NELSON_INTERPROCESS_COMMAND::EVAL_ANSWER;
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
ipcBuiltinTwoRhs(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    // R = ipc(pid, 'minimize')
    ArrayOfVector retval;
    ArrayOf param1 = argIn[0];
    ArrayOf param2 = argIn[1];
    int32 pid = param1.getContentAsInteger32Scalar();
    if (!isPIDRunning(pid)) {
        raiseError(L"Nelson:ipc:ERROR_PID_VALID_EXPECTED", ERROR_PID_VALID_EXPECTED);
    }
    std::wstring commandType = param2.getContentAsWideString();
    if (commandType == L"minimize") {
        nargoutcheck(nLhs, 0, 1);
        std::wstring errorMessage;
        bool r
            = isMinimizedFromNelsonInterprocessReceiver(pid, eval->haveEventsLoop(), errorMessage);
        if (!errorMessage.empty()) {
            raiseError(L"Nelson:ipc:ERROR_COMMAND_NOT_SENT", ERROR_COMMAND_NOT_SENT, errorMessage);
        }
        retval << ArrayOf::logicalConstructor(r);
    } else {
        raiseError(L"Nelson:ipc:ERROR_2_PARAMETER_INVALID_MINIMIZE_PARAMETER_EXPECTED",
            ERROR_2_PARAMETER_INVALID_MINIMIZE_PARAMETER_EXPECTED);
    }
    return retval;
}
//=============================================================================
static ArrayOfVector
ipcBuiltinThreeRhs(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    // ipc(pid, 'post', cmd)
    // ipc(pid_destination, 'eval_answer', cmd_to_evaluate)
    // R = ipc(pid, 'eval', cmd)
    // V = ipc(pid, 'get', name)
    // B = ipc(pid, 'isvar', name)
    // ipc(pid, 'minimize', TF)
    ArrayOfVector retval;
    ArrayOf param1 = argIn[0];
    ArrayOf param2 = argIn[1];
    ArrayOf param3 = argIn[2];
    int32 pid = param1.getContentAsInteger32Scalar();
    if (!isPIDRunning(pid)) {
        raiseError(L"Nelson:ipc:ERROR_PID_VALID_EXPECTED", ERROR_PID_VALID_EXPECTED);
    }
    std::wstring commandType = param2.getContentAsWideString();
    switch (commandTypeToValueSwitch(commandType)) {
    case NELSON_INTERPROCESS_COMMAND::POST_COMMAND: {
        nargoutcheck(nLhs, 0, 0);
        std::wstring command = param3.getContentAsWideString();
        std::wstring errorMessage;
        bool r = postCommandToNelsonInterprocessReceiver(
            pid, command, L"base", eval->haveEventsLoop(), errorMessage);
        if (!r) {
            raiseError(L"Nelson:ipc:ERROR_COMMAND_NOT_SENT", ERROR_COMMAND_NOT_SENT, errorMessage);
        }
    } break;
    case NELSON_INTERPROCESS_COMMAND::EVAL: {
        nargoutcheck(nLhs, 0, 1);
        std::wstring command = param3.getContentAsWideString();
        std::wstring errorMessage;
        std::wstring result;
        bool r = false;
        if (pid == getCurrentPID()) {
            EvaluateConsoleCommandToString(eval, command, result);
            r = true;
        } else {
            r = evalCommandToNelsonInterprocessReceiver(
                pid, command, eval->haveEventsLoop(), result, errorMessage);
        }
        if (!r) {
            raiseError(L"Nelson:ipc:ERROR_COMMAND_NOT_SENT", ERROR_COMMAND_NOT_SENT, errorMessage);
        }
        retval << ArrayOf::characterArrayConstructor(result);
    } break;
    case NELSON_INTERPROCESS_COMMAND::EVAL_ANSWER: {
        std::wstring command = param3.getContentAsWideString();
        std::wstring contentResult;
        std::wstring errorMessage;
        EvaluateConsoleCommandToString(eval, command, contentResult);
        bool r = sendEvalAnswerToNelsonInterprocessReceiver(pid, contentResult);
        if (!r) {
            raiseError(L"Nelson:ipc:ERROR_COMMAND_NOT_SENT", ERROR_COMMAND_NOT_SENT, errorMessage);
        }
    } break;
    case NELSON_INTERPROCESS_COMMAND::SET_MINIMIZE: {
        nargoutcheck(nLhs, 0, 0);
        bool minimize = param3.getContentAsLogicalScalar() != 0U;
        std::wstring errorMessage;
        bool r = sendMinimizeToNelsonInterprocessReceiver(
            pid, minimize, eval->haveEventsLoop(), errorMessage);
        if (!r) {
            raiseError(L"Nelson:ipc:ERROR_COMMAND_NOT_SENT", ERROR_COMMAND_NOT_SENT, errorMessage);
        }
    } break;
    case NELSON_INTERPROCESS_COMMAND::GET: {
        nargoutcheck(nLhs, 0, 1);
        std::wstring name = param3.getContentAsWideString();
        if (!IsValidVariableName(name)) {
            raiseError(L"Nelson:ipc:ERROR_3_ARGUMENT_MUST_CONTAIN_A_VALID_VARIABLE_NAME",
                ERROR_3_ARGUMENT_MUST_CONTAIN_A_VALID_VARIABLE_NAME);
        }
        std::wstring errorMessage;
        ArrayOf result = getVariableFromNelsonInterprocessReceiver(
            pid, name, L"local", eval->haveEventsLoop(), errorMessage);
        if (!errorMessage.empty()) {
            Error(errorMessage, L"Nelson:ipc:getVariableFromNelsonInterprocessReceiver");
        }
        retval << result;
    } break;
    case NELSON_INTERPROCESS_COMMAND::IS_VAR: {
        nargoutcheck(nLhs, 0, 1);
        std::wstring name = param3.getContentAsWideString();
        if (!IsValidVariableName(name)) {
            raiseError(L"Nelson:ipc:ERROR_3_ARGUMENT_MUST_CONTAIN_A_VALID_VARIABLE_NAME",
                ERROR_3_ARGUMENT_MUST_CONTAIN_A_VALID_VARIABLE_NAME);
        }
        std::wstring errorMessage;
        bool result = isVariableFromNelsonInterprocessReceiver(
            pid, name, L"local", eval->haveEventsLoop(), errorMessage);
        if (!errorMessage.empty()) {
            Error(errorMessage, L"Nelson:ipc:isVariableFromNelsonInterprocessReceiver");
        }
        retval << ArrayOf::logicalConstructor(result);
    } break;
    default: {
        raiseError(L"Nelson:ipc:ERROR_2_PARAMETER_INVALID_MINIMIZE_PARAMETER_EXPECTED",
            ERROR_2_PARAMETER_INVALID_MINIMIZE_PARAMETER_EXPECTED);
    } break;
    }
    return retval;
}
//=============================================================================
static ArrayOfVector
ipcBuiltinFourRhs(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    // ipc(pid, 'post', cmd, scope)
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
        raiseError(L"Nelson:ipc:ERROR_PID_VALID_EXPECTED", ERROR_PID_VALID_EXPECTED);
    }
    std::wstring commandType = param2.getContentAsWideString();
    switch (commandTypeToValueSwitch(commandType)) {
    case NELSON_INTERPROCESS_COMMAND::PUT: {
        nargoutcheck(nLhs, 0, 0);
        std::wstring name = param4.getContentAsWideString();
        if (!IsValidVariableName(name)) {
            raiseError(L"Nelson:ipc:ERROR_4_ARGUMENT_MUST_CONTAIN_A_VALID_VARIABLE_NAME",
                ERROR_4_ARGUMENT_MUST_CONTAIN_A_VALID_VARIABLE_NAME);
        }
        std::wstring errorMessage;
        bool r = sendVariableToNelsonInterprocessReceiver(
            pid, param3, name, L"local", eval->haveEventsLoop(), errorMessage);
        if (!r) {
            raiseError(
                L"Nelson:ipc:ERROR_VARIABLE_NOT_SENT", ERROR_VARIABLE_NOT_SENT, errorMessage);
        }
    } break;
    case NELSON_INTERPROCESS_COMMAND::GET: {
        nargoutcheck(nLhs, 0, 1);
        std::wstring name = param3.getContentAsWideString();
        if (!IsValidVariableName(name)) {
            raiseError(L"Nelson:ipc:ERROR_3_ARGUMENT_MUST_CONTAIN_A_VALID_VARIABLE_NAME",
                ERROR_3_ARGUMENT_MUST_CONTAIN_A_VALID_VARIABLE_NAME);
        }
        std::wstring scope = param4.getContentAsWideString();
        bool supported = ((scope == L"global") || (scope == L"base") || (scope == L"caller")
            || (scope == L"local"));
        if (!supported) {
            raiseError(L"Nelson:ipc:ERROR_4_ARGUMENT_MUST_CONTAIN_A_VALID_SCOPE_NAME",
                ERROR_4_ARGUMENT_MUST_CONTAIN_A_VALID_SCOPE_NAME);
        }
        std::wstring errorMessage;
        ArrayOf result = getVariableFromNelsonInterprocessReceiver(
            pid, name, scope, eval->haveEventsLoop(), errorMessage);
        if (!errorMessage.empty()) {
            Error(errorMessage, L"Nelson:ipc:getVariableFromNelsonInterprocessReceiver");
        }
        retval << result;
    } break;
    case NELSON_INTERPROCESS_COMMAND::IS_VAR: {
        nargoutcheck(nLhs, 0, 1);
        std::wstring name = param3.getContentAsWideString();
        if (!IsValidVariableName(name)) {
            raiseError(L"Nelson:ipc:ERROR_3_ARGUMENT_MUST_CONTAIN_A_VALID_VARIABLE_NAME",
                ERROR_3_ARGUMENT_MUST_CONTAIN_A_VALID_VARIABLE_NAME);
        }
        std::wstring scope = param4.getContentAsWideString();
        bool supported = ((scope == L"global") || (scope == L"base") || (scope == L"caller")
            || (scope == L"local"));
        if (!supported) {
            raiseError(L"Nelson:ipc:ERROR_4_ARGUMENT_MUST_CONTAIN_A_VALID_SCOPE_NAME",
                ERROR_4_ARGUMENT_MUST_CONTAIN_A_VALID_SCOPE_NAME);
        }
        std::wstring errorMessage;
        bool result = isVariableFromNelsonInterprocessReceiver(
            pid, name, scope, eval->haveEventsLoop(), errorMessage);
        if (!errorMessage.empty()) {
            Error(errorMessage, L"Nelson:ipc:isVariableFromNelsonInterprocessReceiver");
        }
        retval << ArrayOf::logicalConstructor(result);
    } break;
    case NELSON_INTERPROCESS_COMMAND::POST_COMMAND: {
        nargoutcheck(nLhs, 0, 0);
        std::wstring command = param3.getContentAsWideString();
        std::wstring scope = param4.getContentAsWideString();
        bool supported = ((scope == L"global") || (scope == L"base") || (scope == L"caller")
            || (scope == L"local"));
        if (!supported) {
            raiseError(L"Nelson:ipc:ERROR_4_ARGUMENT_MUST_CONTAIN_A_VALID_SCOPE_NAME",
                ERROR_4_ARGUMENT_MUST_CONTAIN_A_VALID_SCOPE_NAME);
        }
        std::wstring errorMessage;
        bool r = postCommandToNelsonInterprocessReceiver(
            pid, command, scope, eval->haveEventsLoop(), errorMessage);
        if (!r) {
            raiseError(L"Nelson:ipc:ERROR_COMMAND_NOT_SENT", ERROR_COMMAND_NOT_SENT, errorMessage);
        }
    } break;
    default: {
        raiseError(L"Nelson:ipc:ERROR_2_PARAMETER_INVALID_PUT_GET_POST_OR_ISVAR_PARAMETER_EXPECTED",
            ERROR_2_PARAMETER_INVALID_PUT_GET_POST_OR_ISVAR_PARAMETER_EXPECTED);
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
        raiseError(L"Nelson:ipc:ERROR_PID_VALID_EXPECTED", ERROR_PID_VALID_EXPECTED);
    }
    std::wstring commandType = param2.getContentAsWideString();
    switch (commandTypeToValueSwitch(commandType)) {
    case NELSON_INTERPROCESS_COMMAND::PUT: {
        std::wstring name = param4.getContentAsWideString();
        if (!IsValidVariableName(name)) {
            raiseError(L"Nelson:ipc:ERROR_4_ARGUMENT_MUST_CONTAIN_A_VALID_VARIABLE_NAME",
                ERROR_4_ARGUMENT_MUST_CONTAIN_A_VALID_VARIABLE_NAME);
        }
        std::wstring scope = param5.getContentAsWideString();
        bool supported = ((scope == L"global") || (scope == L"base") || (scope == L"caller")
            || (scope == L"local"));
        if (!supported) {
            raiseError(L"Nelson:ipc:ERROR_4_ARGUMENT_MUST_CONTAIN_A_VALID_SCOPE_NAME",
                ERROR_4_ARGUMENT_MUST_CONTAIN_A_VALID_SCOPE_NAME);
        }
        nargoutcheck(nLhs, 0, 0);
        std::wstring errorMessage;
        bool r = sendVariableToNelsonInterprocessReceiver(
            pid, param3, name, scope, eval->haveEventsLoop(), errorMessage);
        if (!r) {
            raiseError(
                L"Nelson:ipc:ERROR_VARIABLE_NOT_SENT", ERROR_VARIABLE_NOT_SENT, errorMessage);
        }
        return retval;
    } break;
    default: {
        raiseError(L"Nelson:ipc:ERROR_2_PARAMETER_INVALID_PUT_PARAMETER_EXPECTED",
            ERROR_2_PARAMETER_INVALID_PUT_PARAMETER_EXPECTED);
    } break;
    }
    return retval;
}
//=============================================================================
// V = ipc(pid, 'eval', cmd)
// ipc(pid, 'post', cmd)
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
    case 2: {
        return ipcBuiltinTwoRhs(eval, nLhs, argIn);
    } break;
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
        if (argIn.size() < 2) {
            raiseError2(L"nelson:arguments:tooFewInputs");
        }
        if (argIn.size() > 5) {
            raiseError2(L"nelson:arguments:tooManyInputs");
        }
    } break;
    }
    return retval;
}
//=============================================================================
