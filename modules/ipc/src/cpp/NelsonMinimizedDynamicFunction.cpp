//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NelsonMinimizedDynamicFunction.hpp"
#include "DynamicLibrary.hpp"
#include "NelsonConfiguration.hpp"
#include <cstdlib>
#include <string>
#include <mutex>
//===================================================================================
namespace {
Nelson::library_handle nlsGuiHandleDynamicLibrary = nullptr;
std::once_flag guiLibraryInitFlag;

template <typename FuncPtr>
FuncPtr
getFunctionPointer(const char* functionName)
{
    return reinterpret_cast<FuncPtr>(
        Nelson::get_function(nlsGuiHandleDynamicLibrary, functionName));
}
}
//===================================================================================
static void
initGuiDynamicLibrary()
{
    std::call_once(guiLibraryInitFlag, []() {
        const std::wstring fullpathGuiSharedLibrary
            = L"libnlsGui" + Nelson::get_dynamic_library_extensionW();
        const std::wstring nelsonLibrariesDirectory
            = Nelson::NelsonConfiguration::getInstance()->getNelsonLibraryDirectory();
        const std::wstring libraryPath = nelsonLibrariesDirectory + L"/" + fullpathGuiSharedLibrary;

        nlsGuiHandleDynamicLibrary = Nelson::load_dynamic_libraryW(libraryPath);
    });
}
//===================================================================================
bool
setNelsonMinimizedDynamicFunction(bool minimize)
{
    using PROC_setNelsonMinimized = bool (*)(bool);
    static PROC_setNelsonMinimized setNelsonMinimizedPtr = nullptr;

    initGuiDynamicLibrary();
    if (setNelsonMinimizedPtr == nullptr) {
        setNelsonMinimizedPtr = getFunctionPointer<PROC_setNelsonMinimized>("setNelsonMinimized");
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
        getNelsonMinimizedPtr = getFunctionPointer<PROC_getNelsonMinimized>("getNelsonMinimized");
        if (getNelsonMinimizedPtr == nullptr) {
            return false;
        }
    }
    return getNelsonMinimizedPtr();
}
//===================================================================================
