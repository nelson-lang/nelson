//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <tuple>
#include <thread>
#include <chrono>
#include "FutureFetchOutputs.hpp"
#include "Error.hpp"
#include "dynamic_library.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static library_handle nlsGuiHandleDynamicLibrary = nullptr;
static bool bFirstDynamicLibraryCall = true;
//=============================================================================
static void
initGuiDynamicLibrary();
static void
ProcessEventsDynamicFunction();
static void
ProcessEventsDynamicFunction(bool bWait);
//=============================================================================
ArrayOfVector
FutureFetchOutputs(Evaluator* eval, FevalFutureObject* fevalFutureObject)
{
    ArrayOfVector result;
    bool wait = true;
    do {
        THREAD_STATE state = fevalFutureObject->getState();
        wait = (state == THREAD_STATE::RUNNING) || (state == THREAD_STATE::QUEUED);
        std::this_thread::sleep_for(std::chrono::milliseconds(10));
        if (eval->haveEventsLoop()) {
            ProcessEventsDynamicFunction();
        }
    } while (wait);
    bool valid = false;
    std::tuple<ArrayOfVector, Exception> resultOrFuture = fevalFutureObject->get(valid);
    result = std::get<0>(resultOrFuture);
    Exception e = std::get<1>(resultOrFuture);
    if (!e.getMessage().empty()) {
        std::wstring errorMessage
            = _W("One or more futures resulted in an error.") + L"\n\n" + e.getMessage();
        Error(errorMessage, L"Nelson:parallel:future:ExecutionError");
    }
    return result;
}
//=============================================================================
void
initGuiDynamicLibrary()
{
    if (bFirstDynamicLibraryCall) {
        std::string fullpathGuiSharedLibrary
            = "libnlsGui" + Nelson::get_dynamic_library_extension();
#ifdef _MSC_VER
        char* buf;
        try {
            buf = new char[MAX_PATH];
        } catch (const std::bad_alloc&) {
            buf = nullptr;
        }
        if (buf != nullptr) {
            DWORD dwRet = ::GetEnvironmentVariableA("NELSON_BINARY_PATH", buf, MAX_PATH);
            if (dwRet != 0U) {
                fullpathGuiSharedLibrary
                    = std::string(buf) + std::string("/") + fullpathGuiSharedLibrary;
            }
            delete[] buf;
        }
#else
        char const* tmp = getenv("NELSON_BINARY_PATH");
        if (tmp != nullptr) {
            fullpathGuiSharedLibrary
                = std::string(tmp) + std::string("/") + fullpathGuiSharedLibrary;
        }
#endif
        nlsGuiHandleDynamicLibrary = Nelson::load_dynamic_library(fullpathGuiSharedLibrary);
        if (nlsGuiHandleDynamicLibrary != nullptr) {
            bFirstDynamicLibraryCall = false;
        }
    }
}
//=============================================================================
void
ProcessEventsDynamicFunction(bool bWait)
{
    using PROC_ProcessEvents = void (*)(bool);
    static PROC_ProcessEvents ProcessEventsPtr = nullptr;
    initGuiDynamicLibrary();
    if (ProcessEventsPtr == nullptr) {
        ProcessEventsPtr = reinterpret_cast<PROC_ProcessEvents>(
            Nelson::get_function(nlsGuiHandleDynamicLibrary, "NelSonProcessEvents"));
    }
    if (ProcessEventsPtr != nullptr) {
        ProcessEventsPtr(bWait);
    }
}
//=============================================================================
void
ProcessEventsDynamicFunction()
{
    ProcessEventsDynamicFunction(false);
}
//=============================================================================
}
//=============================================================================
