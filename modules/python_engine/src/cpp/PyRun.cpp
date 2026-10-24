//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "PyRun.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "PythonLibraryWrapper.hpp"
#include "PythonEnvironment.hpp"
#include "PythonEngine.hpp"
#include "PythonRunner.hpp"
#include "characters_encoding.hpp"
#include "RuntimeDispatch.hpp"
#include "PythonTypesWrapper.hpp"
#include "PythonObjectHandle.hpp"
#include "PyNelsonCallback.hpp"
#include "NelsonConfiguration.hpp"
#include "MainThreadTaskQueue.hpp"
#include <thread>
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOfVector
PyRun(Interface* io, bool haveEventsLoop, const void* voidPythonObjectHandle,
    const wstringVector& commands, const wstringVector& outputs, const wstringVector& names,
    const ArrayOfVector& values)
{
    initializePythonEngine();

    if (isExecutingNelsonPythonCallback()) {
        Error(L"Nelson:Python:ReentrantCallback",
            L"%s cannot be called from within a Nelson function handle that was invoked from "
            L"Python.",
            L"pyrun");
    }

    if (names.size() != values.size()) {
        Error(L"Nelson:Python:nameValueMismatch", L"Same name, value numbers expected.");
    }

    PyObject* pyCode = nullptr;
    if (voidPythonObjectHandle) {
        PythonObjectHandle* poh = (PythonObjectHandle*)voidPythonObjectHandle;
        if (poh->getTypeName() != L"code") {
            Error(L"Nelson:Python:invalidCodeObject", L"Valid Python code object expected.");
        }
        pyCode = (PyObject*)poh->pointer();
    }

    ArrayOfVector retval = {};
    PyObject* globalDict = NLSPyModule_GetDict((PyObject*)getPythonInterpreter());

    for (size_t k = 0; k < names.size(); k++) {
        std::wstring name = names[k];
        ArrayOf value = values[k];
        PyObject* pyValue = arrayOfToPyObject(values[k]);
        NLSPyObject_SetAttrString(
            (PyObject*)getPythonInterpreter(), wstring_to_utf8(name).c_str(), pyValue);
    }
    PythonRunner runner;

    PyThreadState* _save = NLSPyEval_SaveThread();

    std::thread thread([&runner, &globalDict, &pyCode, &commands]() {
        runner.runPythonCode(globalDict, pyCode, commands);
    });

    // Callbacks from pyfunction() are marshalled to this (interpreter) thread through the
    // main-thread task queue: drain it here besides the GUI event pump.
    Evaluator* mainEvaluator
        = static_cast<Evaluator*>(NelsonConfiguration::getInstance()->getMainEvaluator());
    while (!runner.isResultReady()) {
        if (mainEvaluator != nullptr) {
            MainThreadTaskQueue::getInstance()->processTasks(mainEvaluator);
        }
        Nelson::processGuiEvents(false);
    }

    // Join before `runner` (a stack local) is destroyed: a detached thread could call
    // resultCondition.notify_one() after PyRun returned (use-after-free seen in the OS
    // condition-variable wake path).
    if (thread.joinable()) {
        thread.join();
    }

    NLSPyEval_RestoreThread(_save);

    std::wstring outputMessage = runner.getStandardOutput();
    if (!outputMessage.empty()) {
        io->outputMessage(outputMessage);
        io->outputMessage(L"\n");
    }
    std::wstring errorMessage = runner.getErrorMessage();
    if (!errorMessage.empty()) {
        Error(L"Nelson:Python:NelsonException", errorMessage);
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

    return retval;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
