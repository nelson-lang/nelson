//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cstdlib>
#include "NelsonMinimizedDynamicFunction.hpp"
#include "DynamicLibrary.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
static Nelson::library_handle nlsGuiHandleDynamicLibrary = nullptr;
static bool bFirstDynamicLibraryCall = true;
//=============================================================================
static void
initGuiDynamicLibrary()
{
    if (bFirstDynamicLibraryCall) {
        std::wstring fullpathGuiSharedLibrary
            = L"libnlsGui" + Nelson::get_dynamic_library_extensionW();
        std::wstring nelsonLibrariesDirectory
            = Nelson::NelsonConfiguration::getInstance()->getNelsonLibraryDirectory();
        fullpathGuiSharedLibrary
            = nelsonLibrariesDirectory + std::wstring(L"/") + fullpathGuiSharedLibrary;
        nlsGuiHandleDynamicLibrary = Nelson::load_dynamic_libraryW(fullpathGuiSharedLibrary);
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
