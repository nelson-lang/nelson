//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "PythonRunner.hpp"
#include "PythonEngine.hpp"
#include "characters_encoding.hpp"
#include "PythonLibraryWrapper.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
PythonRunner::runPythonCode(PyObject* globalDict, const wstringVector& commands)
{
    outputMessage = L"";
    errorMessage = L"";
    PyGILState_STATE gstate = NLSPyGILState_Ensure();
    for (auto command : commands) {
        int res = NLSPyRun_SimpleStringFlags(wstring_to_utf8(command).c_str(), NULL);
        outputMessage += getPythonStandardOutput();
        if (res != 0) {
            errorMessage = getPythonStandardError();
            NLSPyErr_Clear();
            resultReady = true;
            NLSPyGILState_Release(gstate);
            resultCondition.notify_one();
            return;
        }
    }
    NLSPyGILState_Release(gstate);
    resultReady = true;
    resultCondition.notify_one();
}
//=============================================================================
bool
PythonRunner::isResultReady()
{
    std::lock_guard<std::mutex> lock(resultMutex);
    return resultReady;
}
//=============================================================================
std::wstring
PythonRunner::getErrorMessage()
{
    std::unique_lock<std::mutex> lock(resultMutex);
    resultCondition.wait(lock, [this]() { return resultReady; });
    return errorMessage;
}
//=============================================================================
std::wstring
PythonRunner::getStandardOutput()
{
    std::unique_lock<std::mutex> lock(resultMutex);
    resultCondition.wait(lock, [this]() { return resultReady; });
    return outputMessage;
}
//=============================================================================
}
//=============================================================================
