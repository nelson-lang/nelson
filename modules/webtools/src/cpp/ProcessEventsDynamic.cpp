//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ProcessEventsDynamic.hpp"
#include "DynamicLibrary.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static library_handle nlsGuiHandleDynamicLibrary = nullptr;
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
}
//=============================================================================
