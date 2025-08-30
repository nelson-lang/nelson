//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "MainGuiObjectDynamic.hpp"
#include "DynamicLibrary.hpp"
#include <cstdio>
#include <string>
#include <mutex>
#include <stdexcept>
#include "i18n.hpp"
#include "NelsonConfiguration.hpp"
//===================================================================================
namespace Nelson {
//===================================================================================
namespace {
    //===================================================================================
    library_handle nlsGuiHandleDynamicLibrary = nullptr;
    std::once_flag guiLibraryInitFlag;
    //===================================================================================
    void
    logErrorAndExit(const std::string& message)
    {
        const std::string msg = _(message.c_str()) + "\n";
        fprintf(stderr, "%s", msg.c_str());
        const std::string error_msg = get_dynamic_library_error();
        if (!error_msg.empty()) {
            fprintf(stderr, "%s\n", error_msg.c_str());
        }
        exit(1);
    }
    //===================================================================================
    template <typename FuncPtr>
    FuncPtr
    getFunctionPointer(const char* functionName)
    {
        auto funcPtr
            = reinterpret_cast<FuncPtr>(get_function(nlsGuiHandleDynamicLibrary, functionName));
        if (funcPtr == nullptr) {
            logErrorAndExit(_("Gui Function not loaded."));
        }
        return funcPtr;
    }
    //===================================================================================
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
        if (nlsGuiHandleDynamicLibrary == nullptr) {
            logErrorAndExit("Gui module not loaded.");
        }
    });
}
//===================================================================================
void
InitGuiObjectsDynamic()
{
    using PROC_InitGuiObjects = void (*)();
    static PROC_InitGuiObjects InitGuiObjectsPtr = nullptr;

    initGuiDynamicLibrary();
    if (InitGuiObjectsPtr == nullptr) {
        InitGuiObjectsPtr = getFunctionPointer<PROC_InitGuiObjects>("InitGuiObjects");
    }
    InitGuiObjectsPtr();
}
//===================================================================================
void*
CreateGuiEvaluatorDynamic(void* vcontext, NELSON_ENGINE_MODE mode, bool minimizeWindow, size_t ID)
{
    using PROC_CreateGuiEvaluator = void* (*)(void*, NELSON_ENGINE_MODE, bool, size_t);
    static PROC_CreateGuiEvaluator CreateGuiEvaluatorPtr = nullptr;

    initGuiDynamicLibrary();
    if (CreateGuiEvaluatorPtr == nullptr) {
        CreateGuiEvaluatorPtr = getFunctionPointer<PROC_CreateGuiEvaluator>("CreateGuiEvaluator");
    }
    return CreateGuiEvaluatorPtr(vcontext, mode, minimizeWindow, ID);
}
//===================================================================================
void
DestroyMainGuiObjectDynamic(void* term)
{
    using PROC_DestroyMainGuiObject = void (*)(void*);
    static PROC_DestroyMainGuiObject DestroyMainGuiObjectPtr = nullptr;

    initGuiDynamicLibrary();
    if (DestroyMainGuiObjectPtr == nullptr) {
        DestroyMainGuiObjectPtr
            = getFunctionPointer<PROC_DestroyMainGuiObject>("DestroyMainGuiObject");
    }
    DestroyMainGuiObjectPtr(term);
}
//===================================================================================
void*
GetMainGuiObjectDynamic()
{
    using PROC_GetMainGuiObject = void* (*)();
    static PROC_GetMainGuiObject GetMainGuiObjectPtr = nullptr;

    initGuiDynamicLibrary();
    if (GetMainGuiObjectPtr == nullptr) {
        GetMainGuiObjectPtr = getFunctionPointer<PROC_GetMainGuiObject>("GetMainGuiObject");
    }
    return GetMainGuiObjectPtr();
}
//===================================================================================
} // namespace Nelson
//===================================================================================
