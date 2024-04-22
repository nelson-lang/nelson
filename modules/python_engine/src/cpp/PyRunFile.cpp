//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "PyRunFile.hpp"
#include "PyRun.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "PythonLibraryWrapper.hpp"
#include "PythonEnvironment.hpp"
#include "PythonEngine.hpp"
#include "PythonFileRunner.hpp"
#include "characters_encoding.hpp"
#include "ProcessEventsDynamicFunction.hpp"
#include "PythonTypesWrapper.hpp"
#include "PythonObjectHandle.hpp"
#include <thread>
//=============================================================================
namespace Nelson {
//=============================================================================
static void
passArguments(Interface* io, bool haveEventsLoop, const std::wstring& filename,
    const wstringVector& arguments)
{
    wstringVector commands;
    commands.push_back(L"import sys;");
    std::wstring command = L"sys.argv = [";

    command += L"'";
    command += filename;
    command += L"'";
    if (!arguments.empty()) {
        command += L", ";
    }

    for (size_t i = 0; i < arguments.size(); ++i) {
        command += L"'";
        command += arguments[i];
        command += L"'";
        if (i < arguments.size() - 1) {
            command += L", ";
        }
    }
    command += L"]";
    commands.push_back(command);

    PyRun(io, haveEventsLoop, nullptr, commands, wstringVector(), wstringVector(), ArrayOfVector());
}
//=============================================================================
static void
resetArguments(Interface* io, bool haveEventsLoop)
{
    wstringVector commands;
    commands.push_back(L"sys.argv = [];");
    PyRun(io, haveEventsLoop, nullptr, commands, wstringVector(), wstringVector(), ArrayOfVector());
}
//=============================================================================
ArrayOfVector
PyRunFile(Interface* io, bool haveEventsLoop, const std::wstring& filename,
    const wstringVector& arguments, const wstringVector& outputs, const wstringVector& names,
    const ArrayOfVector& values)
{
    initializePythonEngine();

    if (names.size() != values.size()) {
        Error(_W("Same name, value numbers expected."));
    }

    ArrayOfVector retval = {};

    PyObject* main_module = (PyObject*)getPythonInterpreter();
    PyObject* main_dict = main_module ? NLSPyModule_GetDict(main_module) : nullptr;
    PyObject* main_copy = main_dict ? NLSPyDict_Copy(main_dict) : nullptr;

    for (size_t k = 0; k < names.size(); k++) {
        std::wstring name = names[k];
        ArrayOf value = values[k];
        PyObject* pyValue = arrayOfToPyObject(values[k]);
        NLSPyObject_SetAttrString(
            (PyObject*)getPythonInterpreter(), wstring_to_utf8(name).c_str(), pyValue);
    }

    passArguments(io, haveEventsLoop, filename, arguments);

    PythonFileRunner runner;

    PyThreadState* _save = NLSPyEval_SaveThread();

    std::thread thread([&runner, &filename, &arguments]() { runner.runFile(filename, arguments); });
    thread.detach();

    while (!runner.isResultReady()) {
        ProcessEventsDynamicFunctionWithoutWait();
    }

    NLSPyEval_RestoreThread(_save);

    resetArguments(io, haveEventsLoop);

    std::wstring outputMessage = runner.getStandardOutput();
    if (!outputMessage.empty()) {
        io->outputMessage(outputMessage);
        io->outputMessage(L"\n");
    }
    std::wstring errorMessage = runner.getErrorMessage();
    if (!errorMessage.empty()) {
        if (main_dict) {
            NLSPyDict_Clear(main_dict);
            if (main_copy) {
                NLSPyDict_Update(main_dict, main_copy);
                NLSPy_DECREF(main_copy);
            }
        }
        Error(errorMessage, L"Nelson:Python:Exception");
    }

    for (auto output : outputs) {
        PyObject* pythonObj = NLSPyObject_GetAttrString(
            (PyObject*)getPythonInterpreter(), wstring_to_utf8(output).c_str());
        bool needDecreaseReference;
        retval << PyObjectToArrayOf(pythonObj, needDecreaseReference);
        if (needDecreaseReference) {
            NLSPy_DECREF(pythonObj);
        }
    }
    if (main_dict) {
        NLSPyDict_Clear(main_dict);
        if (main_copy) {
            NLSPyDict_Update(main_dict, main_copy);
            NLSPy_DECREF(main_copy);
        }
    }

    return retval;
}
//=============================================================================
}
//=============================================================================
