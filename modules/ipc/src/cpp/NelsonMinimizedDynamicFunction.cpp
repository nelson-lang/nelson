//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cstdlib>
#include "NelsonMinimizedDynamicFunction.hpp"
#include "DynamicLibrary.hpp"
//=============================================================================
static Nelson::library_handle nlsGuiHandleDynamicLibrary = nullptr;
static bool bFirstDynamicLibraryCall = true;
//=============================================================================
static void
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
bool
setNelsonMinimizedDynamicFunction(bool minimize)
{
    using PROC_setNelsonMinimized = bool (*)(bool);
    static PROC_setNelsonMinimized setNelsonMinimizedPtr = nullptr;
    initGuiDynamicLibrary();
    if (setNelsonMinimizedPtr == nullptr) {
        setNelsonMinimizedPtr = reinterpret_cast<PROC_setNelsonMinimized>(
            Nelson::get_function(nlsGuiHandleDynamicLibrary, "setNelsonMinimized"));
        if (setNelsonMinimizedPtr == nullptr) {
            return false;
        }
    }
    return setNelsonMinimizedPtr(minimize);
}
//===================================================================================
bool
getNelsonMinimizedDynamicFunction()
{
    using PROC_getNelsonMinimized = bool (*)();
    static PROC_getNelsonMinimized getNelsonMinimizedPtr = nullptr;
    initGuiDynamicLibrary();
    if (getNelsonMinimizedPtr == nullptr) {
        getNelsonMinimizedPtr = reinterpret_cast<PROC_getNelsonMinimized>(
            Nelson::get_function(nlsGuiHandleDynamicLibrary, "getNelsonMinimized"));
        if (getNelsonMinimizedPtr == nullptr) {
            return false;
        }
    }
    return getNelsonMinimizedPtr();
}
//===================================================================================
