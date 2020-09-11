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
#include "NelsonConfiguration.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// ipc(pid, 'eval', cmd)
// ipc(pid, 'put', var, name [, scope = current])
// A = {1; 2}; ipc(getpid, 'put', A, 'B')
// V = ipc(pid, 'get', name, scope)
ArrayOfVector
Nelson::EngineGateway::ipcBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (!NelsonConfiguration::getInstance()->isIpcEnabled()) {
        Error(_W("IPC disabled (--noipc command line argument)."));
    }
    if (nLhs != 0) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    int32 pid;
    std::wstring command;
    switch (argIn.size()) {
    case 3: {
        ArrayOf param1 = argIn[0];
        ArrayOf param2 = argIn[1];
        ArrayOf param3 = argIn[2];
        pid = param1.getContentAsInteger32Scalar();
        if (!isPIDRunning(pid)) {
            Error(_W("PID valid expected."));
        }
        std::wstring commandType = param2.getContentAsWideString();
        if (commandType != L"eval") {
            Error(_W("'eval' parameter expected."));
        }
        command = param3.getContentAsWideString();
        bool r = sendCommandToNelsonInterprocessReceiver(pid, command);
        if (!r) {
            Error(_W("Command not sent."));
        }
    } break;
    case 4: {
        ArrayOf param1 = argIn[0]; // pid
        ArrayOf param2 = argIn[1]; // put
        ArrayOf param3 = argIn[2]; // var
        ArrayOf param4 = argIn[3]; // name
        pid = param1.getContentAsInteger32Scalar();
        if (!isPIDRunning(pid)) {
            Error(_W("PID valid expected."));
        }
        std::wstring commandType = param2.getContentAsWideString();
        if (commandType != L"put") {
            Error(_W("'put' parameter expected."));
        }
        std::wstring name = param4.getContentAsWideString();
        if (!IsValidVariableName(name)) {
            Error(_W("#4 Argument must contain a valid variable name."));
        }
        bool r = sendVariableToNelsonInterprocessReceiver(pid, param3, name, L"local");
        if (!r) {
            Error(_W("Variable not sent."));
        }
    } break;
    case 5: {
        ArrayOf param1 = argIn[0]; // pid
        ArrayOf param2 = argIn[1]; // put
        ArrayOf param3 = argIn[2]; // var
        ArrayOf param4 = argIn[3]; // name
        ArrayOf param5 = argIn[4]; // scope
        pid = param1.getContentAsInteger32Scalar();
        if (!isPIDRunning(pid)) {
            Error(_W("PID valid expected."));
        }
        std::wstring commandType = param2.getContentAsWideString();
        if (commandType != L"put") {
            Error(_W("'put' parameter expected."));
        }
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
        bool r = sendVariableToNelsonInterprocessReceiver(pid, param3, name, scope);
        if (!r) {
            Error(_W("Variable not sent."));
        }
    } break;
    default: {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    } break;
    }

    return retval;
}
//=============================================================================
