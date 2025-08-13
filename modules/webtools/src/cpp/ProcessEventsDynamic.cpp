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
#include <string>
#include <mutex>
//===================================================================================
namespace Nelson {
//===================================================================================
namespace {
    library_handle nlsGuiHandleDynamicLibrary = nullptr;
    std::once_flag guiLibraryInitFlag;

    template <typename FuncPtr>
    FuncPtr
    getFunctionPointer(const char* functionName)
    {
        return reinterpret_cast<FuncPtr>(get_function(nlsGuiHandleDynamicLibrary, functionName));
    }
}
//===================================================================================
static void
initGuiDynamicLibrary()
{
    std::call_once(guiLibraryInitFlag, []() {
        const std::wstring fullpathGuiSharedLibrary
            = L"libnlsGui" + get_dynamic_library_extensionW();
        const std::wstring nelsonLibrariesDirectory
            = NelsonConfiguration::getInstance()->getNelsonLibraryDirectory();
        const std::wstring libraryPath = nelsonLibrariesDirectory + L"/" + fullpathGuiSharedLibrary;

        nlsGuiHandleDynamicLibrary = load_dynamic_libraryW(libraryPath);
    });
}
//===================================================================================
void
ProcessEventsDynamicFunction(bool bWait)
{
    using PROC_ProcessEvents = void (*)(bool);
    static PROC_ProcessEvents ProcessEventsPtr = nullptr;
    initGuiDynamicLibrary();
    if (ProcessEventsPtr == nullptr) {
        ProcessEventsPtr = getFunctionPointer<PROC_ProcessEvents>("NelSonProcessEvents");
    }
    if (ProcessEventsPtr != nullptr) {
        ProcessEventsPtr(bWait);
    }
}
//===================================================================================
} // namespace Nelson
//===================================================================================
