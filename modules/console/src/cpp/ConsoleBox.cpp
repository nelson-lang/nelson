//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ConsoleBox.hpp"
#ifdef _MSC_VER
#include "DynamicLibrary.hpp"
#include "NelsonConfiguration.hpp"
#include <mutex>
#include <filesystem>
#include <string>
#endif
//=============================================================================
namespace Nelson {
//=============================================================================
#ifdef _MSC_VER
static bool
initGuiDynamicLibrary();
//=============================================================================
Nelson::library_handle nlsGuiHandleDynamicLibrary = nullptr;
std::once_flag guiLibraryInitFlag;
//=============================================================================
template <typename FuncPtr>
FuncPtr
getFunctionPointer(const char* functionName)
{
    // return nullptr if library wasn't loaded
    if (nlsGuiHandleDynamicLibrary == nullptr) {
        return nullptr;
    }
    return reinterpret_cast<FuncPtr>(
        Nelson::get_function(nlsGuiHandleDynamicLibrary, functionName));
}
#endif
//=============================================================================
bool
ConsoleBoxShow(bool show)
{
#ifdef _MSC_VER
    using PROC_ShowConsoleWindow = bool (*)(bool);
    static PROC_ShowConsoleWindow ShowConsoleWindowPtr = nullptr;

    if (!initGuiDynamicLibrary()) {
        return false;
    }
    if (ShowConsoleWindowPtr == nullptr) {
        ShowConsoleWindowPtr = getFunctionPointer<PROC_ShowConsoleWindow>("ShowConsoleWindow");
    }
    if (ShowConsoleWindowPtr != nullptr) {
        ShowConsoleWindowPtr(show);
        // return current visible state (keep previous behavior)
        return ConsoleBoxIsVisible();
    }
    return false;
#endif
    return false;
}
//=============================================================================
bool
ConsoleBoxToggle()
{
#ifdef _MSC_VER
    using PROC_ToggleConsole = bool (*)();
    static PROC_ToggleConsole ToggleConsolePtr = nullptr;

    if (!initGuiDynamicLibrary()) {
        return false;
    }
    if (ToggleConsolePtr == nullptr) {
        ToggleConsolePtr = getFunctionPointer<PROC_ToggleConsole>("ToggleConsole");
    }
    if (ToggleConsolePtr != nullptr) {
        ToggleConsolePtr();
        return ConsoleBoxIsVisible();
    }
    return false;
#endif
    return false;
}
//=============================================================================
bool
ConsoleBoxIsVisible()
{
#ifdef _MSC_VER
    using PROC_ConsoleIsVisible = bool (*)();
    static PROC_ConsoleIsVisible ConsoleIsVisiblePtr = nullptr;

    if (!initGuiDynamicLibrary()) {
        return false;
    }
    if (ConsoleIsVisiblePtr == nullptr) {
        ConsoleIsVisiblePtr = getFunctionPointer<PROC_ConsoleIsVisible>("ConsoleIsVisible");
    }
    if (ConsoleIsVisiblePtr != nullptr) {
        return ConsoleIsVisiblePtr();
    }
#endif
    return false;
}

//=============================================================================
#ifdef _MSC_VER
bool
initGuiDynamicLibrary()
{
    std::call_once(guiLibraryInitFlag, []() {
        const std::wstring fullpathGuiSharedLibrary
            = L"libnlsGui" + Nelson::get_dynamic_library_extensionW();
        const std::wstring nelsonLibrariesDirectory
            = Nelson::NelsonConfiguration::getInstance()->getNelsonLibraryDirectory();

        std::filesystem::path libDir(nelsonLibrariesDirectory);
        std::filesystem::path libraryPath = libDir / fullpathGuiSharedLibrary;

        nlsGuiHandleDynamicLibrary = Nelson::load_dynamic_libraryW(libraryPath.wstring());
    });
    return (nlsGuiHandleDynamicLibrary != nullptr);
}
#endif
//=============================================================================
}
//=============================================================================
