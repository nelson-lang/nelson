//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS /* _wfopen */
#endif
//=============================================================================
#include "PythonFileRunner.hpp"
#include "PythonEngine.hpp"
#include "characters_encoding.hpp"
#include "PythonLibraryWrapper.hpp"
#include "i18n.hpp"
#include <cstdio>
#include <algorithm>
//=============================================================================
namespace Nelson {
//=============================================================================
void
PythonFileRunner::runFile(const std::wstring& filename, const wstringVector& arguments)
{
    outputMessage = L"";
    errorMessage = L"";

    PyGILState_STATE gstate = NLSPyGILState_Ensure();

    PyObject* obj = NLSPy_BuildValue("s", wstring_to_utf8(filename).c_str());
    FILE* file = NLS_Py_fopen_obj(obj, "r+");
    NLSPy_DECREF(obj);
    if (file == NULL) {
        errorMessage = _W("Impossible to open file: ") + filename;
        NLSPyGILState_Release(gstate);
        resultReady = true;
        resultCondition.notify_one();

        return;
    }

    int ret = NLSPyRun_SimpleFileExFlags(file, wstring_to_utf8(filename).c_str(), 1, NULL);
    outputMessage += getPythonStandardOutput();
    std::wstring msg = getPythonStandardError();
    if (ret != 0) {
        errorMessage += msg;
        NLSPyErr_Clear();
    }
    if (!msg.empty()) {
        outputMessage += msg;
        NLSPyErr_Clear();
    }

    NLSPyGILState_Release(gstate);

    resultReady = true;
    resultCondition.notify_one();
}
//=============================================================================
bool
PythonFileRunner::isResultReady()
{
    std::lock_guard<std::mutex> lock(resultMutex);
    return resultReady;
}
//=============================================================================
std::wstring
PythonFileRunner::getErrorMessage()
{
    std::unique_lock<std::mutex> lock(resultMutex);
    resultCondition.wait(lock, [this]() { return resultReady; });
    return errorMessage;
}
//=============================================================================
std::wstring
PythonFileRunner::getStandardOutput()
{
    std::unique_lock<std::mutex> lock(resultMutex);
    resultCondition.wait(lock, [this]() { return resultReady; });
    return outputMessage;
}
//=============================================================================
}
//=============================================================================
