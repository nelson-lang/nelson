//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "CallExportGraphicsGui.hpp"
#include "DynamicLibrary.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
static Nelson::library_handle nlsGraphicsIoHandleDynamicLibrary = nullptr;
static bool bFirstDynamicLibraryCall = true;
//=============================================================================
namespace Nelson {
//=============================================================================
static void
initGraphicsIoDynamicLibrary()
{
    if (bFirstDynamicLibraryCall) {
        std::wstring fullpathGuiSharedLibrary
            = L"libnlsGraphics_io" + Nelson::get_dynamic_library_extensionW();
        std::wstring nelsonLibrariesDirectory
            = Nelson::NelsonConfiguration::getInstance()->getNelsonLibraryDirectory();
        fullpathGuiSharedLibrary
            = nelsonLibrariesDirectory + std::wstring(L"/") + fullpathGuiSharedLibrary;
        nlsGraphicsIoHandleDynamicLibrary = Nelson::load_dynamic_libraryW(fullpathGuiSharedLibrary);
        if (nlsGraphicsIoHandleDynamicLibrary != nullptr) {
            bFirstDynamicLibraryCall = false;
        }
    }
}
//=============================================================================
bool
callExportGraphicsGui(GOWindow* goWindow)
{
    using PROC_ExportGraphicsGUI = bool (*)(void*);
    static PROC_ExportGraphicsGUI ExportGraphicsGUIPtr = nullptr;
    initGraphicsIoDynamicLibrary();
    if (ExportGraphicsGUIPtr == nullptr) {
        ExportGraphicsGUIPtr = reinterpret_cast<PROC_ExportGraphicsGUI>(
            Nelson::get_function(nlsGraphicsIoHandleDynamicLibrary, "ExportGraphicsGUI"));
    }
    if (ExportGraphicsGUIPtr != nullptr) {
        return ExportGraphicsGUIPtr((void*)goWindow);
    }
    return false;
}
//=============================================================================
}
//=============================================================================
