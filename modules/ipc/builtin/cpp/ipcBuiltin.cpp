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
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "NelsonInterprocess.hpp"
#include "NelsonMinimizedDynamicFunction.hpp"
#include "IsValidVariableName.hpp"
#include "NelsonPIDs.hpp"
#include "EvaluateCommand.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static bool
isCurrentGuiProcess(int32 pid)
{
    return pid == getCurrentPID()
        && NelsonConfiguration::getInstance()->getNelsonEngineMode() == NELSON_ENGINE_MODE::GUI;
}
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
    int32 pid = param1.asInt32Scalar();
    if (!isPIDRunning(pid)) {
        Error(L"Nelson:IPC:invalidPID", L"PID valid expected.");
    }
    std::wstring commandType = param2.asWideString();
    if (commandType == L"minimize") {
        nargoutcheck(nLhs, 0, 1);
        if (isCurrentGuiProcess(pid)) {
            retval << ArrayOf::logicalScalar(getNelsonMinimizedDynamicFunction());
            return retval;
        }
        std::wstring errorMessage;
        bool r
            = isMinimizedFromNelsonInterprocessReceiver(pid, eval->haveEventsLoop(), errorMessage);
        if (!errorMessage.empty()) {
            Error(L"Nelson:IPC:commandNotSent", L"Command not sent.%s", errorMessage);
        }
        retval << ArrayOf::logicalScalar(r);
    } else {
        Error(L"Nelson:IPC:invalidParameter", L"#%d argument invalid: %s parameter expected.", 2,
            L"'minimize'");
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
    int32 pid = param1.asInt32Scalar();
    if (!isPIDRunning(pid)) {
        Error(L"Nelson:IPC:invalidPID", L"PID valid expected.");
    }
    std::wstring commandType = param2.asWideString();
    switch (commandTypeToValueSwitch(commandType)) {
    case NELSON_INTERPROCESS_COMMAND::POST_COMMAND: {
        nargoutcheck(nLhs, 0, 0);
        std::wstring command = param3.asWideString();
        std::wstring errorMessage;
        bool r = postCommandToNelsonInterprocessReceiver(
            pid, command, L"base", eval->haveEventsLoop(), errorMessage);
        if (!r) {
            Error(L"Nelson:IPC:commandNotSent", L"Command not sent.%s", errorMessage);
        }
    } break;
    case NELSON_INTERPROCESS_COMMAND::EVAL: {
        nargoutcheck(nLhs, 0, 1);
        std::wstring command = param3.asWideString();
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
            Error(L"Nelson:IPC:commandNotSent", L"Command not sent.%s", errorMessage);
        }
        retval << ArrayOf::characterArray(result);
    } break;
    case NELSON_INTERPROCESS_COMMAND::EVAL_ANSWER: {
        std::wstring command = param3.asWideString();
        std::wstring contentResult;
        std::wstring errorMessage;
        EvaluateConsoleCommandToString(eval, command, contentResult);
        bool r = sendEvalAnswerToNelsonInterprocessReceiver(pid, contentResult);
        if (!r) {
            Error(L"Nelson:IPC:commandNotSent", L"Command not sent.%s", errorMessage);
        }
    } break;
    case NELSON_INTERPROCESS_COMMAND::SET_MINIMIZE: {
        nargoutcheck(nLhs, 0, 0);
        bool minimize = param3.asLogicalScalar() != 0U;
        if (isCurrentGuiProcess(pid)) {
            if (!setNelsonMinimizedDynamicFunction(minimize)) {
                Error(L"Nelson:IPC:commandNotSent", L"Command not sent.%s", L"");
            }
            break;
        }
        std::wstring errorMessage;
        bool r = sendMinimizeToNelsonInterprocessReceiver(
            pid, minimize, eval->haveEventsLoop(), errorMessage);
        if (!r) {
            Error(L"Nelson:IPC:commandNotSent", L"Command not sent.%s", errorMessage);
        }
    } break;
    case NELSON_INTERPROCESS_COMMAND::GET: {
        nargoutcheck(nLhs, 0, 1);
        std::wstring name = param3.asWideString();
        if (!IsValidVariableName(name)) {
            Error(L"Nelson:IPC:invalidVariableName",
                L"#%d argument must contain a valid variable name.", 3);
        }
        std::wstring errorMessage;
        ArrayOf result = getVariableFromNelsonInterprocessReceiver(
            pid, name, L"local", eval->haveEventsLoop(), errorMessage);
        if (!errorMessage.empty()) {
            Error(L"Nelson:IPC:operationFailed", errorMessage);
        }
        retval << result;
    } break;
    case NELSON_INTERPROCESS_COMMAND::IS_VAR: {
        nargoutcheck(nLhs, 0, 1);
        std::wstring name = param3.asWideString();
        if (!IsValidVariableName(name)) {
            Error(L"Nelson:IPC:invalidVariableName",
                L"#%d argument must contain a valid variable name.", 3);
        }
        std::wstring errorMessage;
        bool result = isVariableFromNelsonInterprocessReceiver(
            pid, name, L"local", eval->haveEventsLoop(), errorMessage);
        if (!errorMessage.empty()) {
            Error(L"Nelson:IPC:operationFailed", errorMessage);
        }
        retval << ArrayOf::logicalScalar(result);
    } break;
    default: {
        Error(L"Nelson:IPC:invalidParameter", L"#%d argument invalid: %s parameter expected.", 2,
            L"'post', 'eval', 'put', 'get', 'minimize' or 'isvar'");
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
    pid = param1.asInt32Scalar();
    if (!isPIDRunning(pid)) {
        Error(L"Nelson:IPC:invalidPID", L"PID valid expected.");
    }
    std::wstring commandType = param2.asWideString();
    switch (commandTypeToValueSwitch(commandType)) {
    case NELSON_INTERPROCESS_COMMAND::PUT: {
        nargoutcheck(nLhs, 0, 0);
        std::wstring name = param4.asWideString();
        if (!IsValidVariableName(name)) {
            Error(L"Nelson:IPC:invalidVariableName",
                L"#%d argument must contain a valid variable name.", 4);
        }
        std::wstring errorMessage;
        bool r = sendVariableToNelsonInterprocessReceiver(
            pid, param3, name, L"local", eval->haveEventsLoop(), errorMessage);
        if (!r) {
            Error(L"Nelson:IPC:variableNotSent", L"Variable not sent.%s", errorMessage);
        }
    } break;
    case NELSON_INTERPROCESS_COMMAND::GET: {
        nargoutcheck(nLhs, 0, 1);
        std::wstring name = param3.asWideString();
        if (!IsValidVariableName(name)) {
            Error(L"Nelson:IPC:invalidVariableName",
                L"#%d argument must contain a valid variable name.", 3);
        }
        std::wstring scope = param4.asWideString();
        bool supported = ((scope == L"global") || (scope == L"base") || (scope == L"caller")
            || (scope == L"local"));
        if (!supported) {
            Error(L"Nelson:IPC:invalidScopeName", L"#%d argument must contain a valid scope name.",
                4);
        }
        std::wstring errorMessage;
        ArrayOf result = getVariableFromNelsonInterprocessReceiver(
            pid, name, scope, eval->haveEventsLoop(), errorMessage);
        if (!errorMessage.empty()) {
            Error(L"Nelson:IPC:operationFailed", errorMessage);
        }
        retval << result;
    } break;
    case NELSON_INTERPROCESS_COMMAND::IS_VAR: {
        nargoutcheck(nLhs, 0, 1);
        std::wstring name = param3.asWideString();
        if (!IsValidVariableName(name)) {
            Error(L"Nelson:IPC:invalidVariableName",
                L"#%d argument must contain a valid variable name.", 3);
        }
        std::wstring scope = param4.asWideString();
        bool supported = ((scope == L"global") || (scope == L"base") || (scope == L"caller")
            || (scope == L"local"));
        if (!supported) {
            Error(L"Nelson:IPC:invalidScopeName", L"#%d argument must contain a valid scope name.",
                4);
        }
        std::wstring errorMessage;
        bool result = isVariableFromNelsonInterprocessReceiver(
            pid, name, scope, eval->haveEventsLoop(), errorMessage);
        if (!errorMessage.empty()) {
            Error(L"Nelson:IPC:operationFailed", errorMessage);
        }
        retval << ArrayOf::logicalScalar(result);
    } break;
    case NELSON_INTERPROCESS_COMMAND::POST_COMMAND: {
        nargoutcheck(nLhs, 0, 0);
        std::wstring command = param3.asWideString();
        std::wstring scope = param4.asWideString();
        bool supported = ((scope == L"global") || (scope == L"base") || (scope == L"caller")
            || (scope == L"local"));
        if (!supported) {
            Error(L"Nelson:IPC:invalidScopeName", L"#%d argument must contain a valid scope name.",
                4);
        }
        std::wstring errorMessage;
        bool r = postCommandToNelsonInterprocessReceiver(
            pid, command, scope, eval->haveEventsLoop(), errorMessage);
        if (!r) {
            Error(L"Nelson:IPC:commandNotSent", L"Command not sent.%s", errorMessage);
        }
    } break;
    default: {
        Error(L"Nelson:IPC:invalidParameter", L"#%d argument invalid: %s parameter expected.", 2,
            L"'put', 'get', 'post' or 'isvar'");
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
    int32 pid = param1.asInt32Scalar();
    if (!isPIDRunning(pid)) {
        Error(L"Nelson:IPC:invalidPID", L"PID valid expected.");
    }
    std::wstring commandType = param2.asWideString();
    switch (commandTypeToValueSwitch(commandType)) {
    case NELSON_INTERPROCESS_COMMAND::PUT: {
        std::wstring name = param4.asWideString();
        if (!IsValidVariableName(name)) {
            Error(L"Nelson:IPC:invalidVariableName",
                L"#%d argument must contain a valid variable name.", 4);
        }
        std::wstring scope = param5.asWideString();
        bool supported = ((scope == L"global") || (scope == L"base") || (scope == L"caller")
            || (scope == L"local"));
        if (!supported) {
            Error(L"Nelson:IPC:invalidScopeName", L"#%d argument must contain a valid scope name.",
                4);
        }
        nargoutcheck(nLhs, 0, 0);
        std::wstring errorMessage;
        bool r = sendVariableToNelsonInterprocessReceiver(
            pid, param3, name, scope, eval->haveEventsLoop(), errorMessage);
        if (!r) {
            Error(L"Nelson:IPC:variableNotSent", L"Variable not sent.%s", errorMessage);
        }
        return retval;
    } break;
    default: {
        Error(L"Nelson:IPC:invalidParameter", L"#%d argument invalid: %s parameter expected.", 2,
            L"'put'");
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
    nargincheck(argIn, 2, 5);
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
    }
    return retval;
}
//=============================================================================
