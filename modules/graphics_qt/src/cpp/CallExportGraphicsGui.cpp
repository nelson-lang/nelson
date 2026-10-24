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
static Nelson::library_handle nlsGraphicsIoQtHandleDynamicLibrary = nullptr;
static bool bFirstDynamicLibraryCall = true;
//=============================================================================
namespace Nelson {
//=============================================================================
static void
initGraphicsIoDynamicLibrary()
{
    if (bFirstDynamicLibraryCall) {
        std::wstring nelsonLibrariesDirectory
            = Nelson::NelsonConfiguration::getInstance()->getNelsonLibraryDirectory();
        // Load each library at most once: reloading on every export attempt
        // would overwrite (and leak) the previous handle when only one of the
        // two libraries is present.
        if (nlsGraphicsIoHandleDynamicLibrary == nullptr) {
            std::wstring fullpathGuiSharedLibrary = nelsonLibrariesDirectory + std::wstring(L"/")
                + L"libnlsGraphics_io" + Nelson::get_dynamic_library_extensionW();
            nlsGraphicsIoHandleDynamicLibrary
                = Nelson::load_dynamic_libraryW(fullpathGuiSharedLibrary);
        }
        if (nlsGraphicsIoQtHandleDynamicLibrary == nullptr) {
            std::wstring fullpathGuiQtSharedLibrary = nelsonLibrariesDirectory + std::wstring(L"/")
                + L"libnlsGraphics_io_qt" + Nelson::get_dynamic_library_extensionW();
            nlsGraphicsIoQtHandleDynamicLibrary
                = Nelson::load_dynamic_libraryW(fullpathGuiQtSharedLibrary);
        }
        if (nlsGraphicsIoHandleDynamicLibrary != nullptr
            && nlsGraphicsIoQtHandleDynamicLibrary != nullptr) {
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
        // GOWindow derives from QMainWindow *and* GOFigureHost; GOFigureHost is not the
        // primary base, so its subobject sits at a non-zero offset. Upcast through the typed
        // pointer first so the compiler applies that offset, then erase to void*. Casting
        // the raw GOWindow* to void* would hand the other side of the C ABI boundary the
        // QMainWindow subobject, and its static_cast<GOFigureHost*> and
        // dynamic_cast<GOWindow*> would fail.
        GOFigureHost* host = goWindow;
        return ExportGraphicsGUIPtr(static_cast<void*>(host));
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
