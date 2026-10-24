//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "PyNelsonCallback.hpp"
#include "PythonLibraryWrapper.hpp"
#include "PythonTypesWrapper.hpp"
#include "PythonEngine.hpp"
#include "Types.hpp"
#include "Evaluator.hpp"
#include "FunctionDef.hpp"
#include "NelsonException.hpp"
#include "NelsonConfiguration.hpp"
#include "MainThreadTaskQueue.hpp"
#include "characters_encoding.hpp"
#include <atomic>
#include <cstdint>
#include <mutex>
#include <thread>
#include <unordered_map>
//=============================================================================
namespace Nelson {
//=============================================================================
// Function handles kept alive while Python can still call back, keyed by the id carried
// in the proxy's PyCapsule.
static std::mutex registryMutex;
static std::unordered_map<int64_t, ArrayOf> callbackRegistry;
static int64_t nextCallbackId = 1;
//=============================================================================
// Only the interpreter thread may run the evaluator. It is captured when the first
// callable is built (builtins always run on that thread).
static std::thread::id interpreterThreadId;
static bool interpreterThreadIdSet = false;
//=============================================================================
// True while the interpreter thread runs a callback from Python. pyrun/pyrunfile read it
// to reject re-entrant calls instead of deadlocking.
static std::atomic<bool> executingCallback { false };
//=============================================================================
static const char* NELSON_CALLBACK_CAPSULE_NAME = "nelson.function_handle.id";
//=============================================================================
struct CallbackInvocation
{
    ArrayOf functionHandle;
    ArrayOfVector args;
    ArrayOf result;
    bool producedOutput = false;
    bool hadError = false;
    std::wstring errorMessage;
};
//=============================================================================
bool
isExecutingNelsonPythonCallback()
{
    return executingCallback.load();
}
//=============================================================================
// Never throws: any Nelson error is captured in the invocation.
static void
runInvocationOnInterpreterThread(Evaluator* eval, CallbackInvocation& invocation)
{
    executingCallback.store(true);
    try {
        if (!invocation.functionHandle.isFunctionHandle()) {
            invocation.hadError = true;
            invocation.errorMessage = L"Invalid function handle.";
        } else {
            function_handle fh = invocation.functionHandle.asFunctionHandle();
            FunctionDef* funcDef = reinterpret_cast<FunctionDef*>(fh.anonymousHandle);
            if (funcDef == nullptr) {
                invocation.hadError = true;
                invocation.errorMessage = L"Invalid function handle.";
            } else {
                // Ask for one output: an anonymous handle only yields its body value then.
                // A handle whose body returns nothing (@() error('boom')) rejects that
                // request before running the body; retry with no output so the real body
                // error surfaces instead of "Wrong number of output arguments".
                ArrayOfVector out;
                try {
                    out = funcDef->evaluateFunction(eval, invocation.args, 1);
                } catch (const NelsonException&) {
                    out = funcDef->evaluateFunction(eval, invocation.args, 0);
                }
                if (!out.empty()) {
                    invocation.result = out[0];
                    invocation.producedOutput = true;
                }
            }
        }
    } catch (const NelsonException& e) {
        invocation.hadError = true;
        invocation.errorMessage = e.message();
    } catch (...) {
        invocation.hadError = true;
        invocation.errorMessage = L"Unexpected error while evaluating the Nelson callback.";
    }
    executingCallback.store(false);
}
//=============================================================================
static PyObject*
raisePythonRuntimeError(const std::wstring& message)
{
    std::string utf8 = wstring_to_utf8(message);
    NLSPyErr_SetString(NLSPyExc_RuntimeError(), utf8.c_str());
    return nullptr;
}
//=============================================================================
// Runs with the GIL held. No C++ exception may escape into CPython.
static PyObject*
nelsonCallbackTrampoline(PyObject* self, PyObject* args)
{
    try {
        int64_t id = (int64_t)(intptr_t)NLSPyCapsule_GetPointer(self, NELSON_CALLBACK_CAPSULE_NAME);

        CallbackInvocation invocation;
        {
            std::lock_guard<std::mutex> lock(registryMutex);
            auto it = callbackRegistry.find(id);
            if (it == callbackRegistry.end()) {
                return raisePythonRuntimeError(L"Nelson callback is no longer available.");
            }
            invocation.functionHandle = it->second;
        }

        // Items are borrowed from the argument tuple: PyObjectToArrayOf takes its own
        // reference when it wraps a handle.
        Py_ssize_t nargs = (args != nullptr) ? NLSPyTuple_Size(args) : 0;
        for (Py_ssize_t k = 0; k < nargs; ++k) {
            PyObject* item = NLSPyTuple_GetItem(args, k);
            bool needToDecreaseReference = false;
            invocation.args.push_back(PyObjectToArrayOf(item, needToDecreaseReference));
        }

        bool onInterpreterThread = false;
        {
            std::lock_guard<std::mutex> lock(registryMutex);
            onInterpreterThread
                = interpreterThreadIdSet && (std::this_thread::get_id() == interpreterThreadId);
        }

        if (onInterpreterThread) {
            // Re-entrant call on the interpreter thread: run in place with the GIL held.
            Evaluator* eval
                = static_cast<Evaluator*>(NelsonConfiguration::getInstance()->getMainEvaluator());
            if (eval == nullptr) {
                return raisePythonRuntimeError(L"Nelson evaluator is not available.");
            }
            runInvocationOnInterpreterThread(eval, invocation);
        } else {
            // Worker or foreign thread: marshal to the interpreter thread and block. The
            // GIL is released meanwhile because the callback may touch Python objects
            // (e.g. a numpy argument), so the interpreter thread must be able to take it.
            PyThreadState* savedThreadState = NLSPyEval_SaveThread();
            bool ran = MainThreadTaskQueue::getInstance()->runOnInterpreterThread(
                [&invocation](Evaluator* eval) {
                    PyGILState_STATE gilState = NLSPyGILState_Ensure();
                    runInvocationOnInterpreterThread(eval, invocation);
                    NLSPyGILState_Release(gilState);
                },
                60000);
            NLSPyEval_RestoreThread(savedThreadState);
            if (!ran) {
                return raisePythonRuntimeError(
                    L"Nelson callback could not reach the interpreter thread. The Nelson session "
                    L"is single-threaded: call it from the main thread only (for example use "
                    L"n_jobs=1).");
            }
        }

        if (invocation.hadError) {
            return raisePythonRuntimeError(invocation.errorMessage);
        }
        if (!invocation.producedOutput) {
            PyObject* none = Py_None;
            NLSPy_INCREF(none);
            return none;
        }
        return arrayOfToPyObject(invocation.result);
    } catch (const NelsonException& e) {
        return raisePythonRuntimeError(e.message());
    } catch (...) {
        return raisePythonRuntimeError(L"Unexpected error in the Nelson callback.");
    }
}
//=============================================================================
static PyMethodDef nelsonCallbackMethodDef
    = { "__nelson_invoke__", (PyCFunction)nelsonCallbackTrampoline, METH_VARARGS,
          "Invoke a Nelson function handle from Python." };
//=============================================================================
static void
nelsonCallbackCapsuleDestructor(PyObject* capsule)
{
    int64_t id = (int64_t)(intptr_t)NLSPyCapsule_GetPointer(capsule, NELSON_CALLBACK_CAPSULE_NAME);
    std::lock_guard<std::mutex> lock(registryMutex);
    callbackRegistry.erase(id);
}
//=============================================================================
void*
createNelsonCallable(const ArrayOf& functionHandle)
{
    int64_t id = 0;
    {
        std::lock_guard<std::mutex> lock(registryMutex);
        if (!interpreterThreadIdSet) {
            interpreterThreadId = std::this_thread::get_id();
            interpreterThreadIdSet = true;
        }
        id = nextCallbackId++;
        callbackRegistry[id] = functionHandle;
    }

    PyObject* capsule = NLSPyCapsule_New(
        (void*)(intptr_t)id, NELSON_CALLBACK_CAPSULE_NAME, nelsonCallbackCapsuleDestructor);
    if (capsule == nullptr) {
        std::lock_guard<std::mutex> lock(registryMutex);
        callbackRegistry.erase(id);
        return nullptr;
    }

    // The C function object owns the only reference to the capsule; its destructor drops
    // the registry entry.
    PyObject* callable = NLSPyCFunction_NewEx(&nelsonCallbackMethodDef, capsule, nullptr);
    NLSPy_DECREF(capsule);
    if (callable == nullptr) {
        std::lock_guard<std::mutex> lock(registryMutex);
        callbackRegistry.erase(id);
        return nullptr;
    }
    return (void*)callable;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
