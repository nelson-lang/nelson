//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "PyRun.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "PythonLibraryWrapper.hpp"
#include "PythonEnvironment.hpp"
#include "PythonEngine.hpp"
#include "PythonRunner.hpp"
#include "characters_encoding.hpp"
#include "ProcessEventsDynamicFunction.hpp"
#include "PythonTypesWrapper.hpp"
#include <thread>
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOfVector
PyRun(Interface* io, bool haveEventsLoop, const wstringVector& commands,
    const wstringVector& outputs, const wstringVector& names, const ArrayOfVector& values)
{
    initializePythonEngine();

    if (names.size() != values.size()) {
        Error(_W("Same name, value numbers expected."));
    }

    ArrayOfVector retval = {};
    PyObject* globalDict = NLSPyModule_GetDict((PyObject*)getPythonInterpreter());

    for (size_t k = 0; k < names.size(); k++) {
        std::wstring name = names[k];
        ArrayOf value = values[k];
        PyObject* pyValue = arrayOfToPyObject(values[k]);
        int result = NLSPyObject_SetAttrString(
            (PyObject*)getPythonInterpreter(), wstring_to_utf8(name).c_str(), pyValue);
    }
    PythonRunner runner;

    PyThreadState* _save = NLSPyEval_SaveThread();

    std::thread thread(
        [&runner, &globalDict, &commands]() { runner.runPythonCode(globalDict, commands); });
    thread.detach();

    while (!runner.isResultReady()) {
        ProcessEventsDynamicFunctionWithoutWait();
    }

    NLSPyEval_RestoreThread(_save);

    std::wstring outputMessage = runner.getStandardOutput();
    if (!outputMessage.empty()) {
        io->outputMessage(outputMessage);
        io->outputMessage(L"\n");
    }
    std::wstring errorMessage = runner.getErrorMessage();
    if (!errorMessage.empty()) {
        Error(errorMessage, L"Nelson:Python:Exception");
    }

    for (auto output : outputs) {
        PyObject* pythonObj = NLSPyObject_GetAttrString(
            (PyObject*)getPythonInterpreter(), wstring_to_utf8(output).c_str());
        retval << PyObjectToArrayOf(pythonObj);
    }

    return retval;
}
//=============================================================================
}
//=============================================================================
