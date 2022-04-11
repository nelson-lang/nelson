//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "MainGuiObjectDynamic.hpp"
#include "dynamic_library.hpp"
#include "i18n.hpp"
#include <boost/function.hpp>
#include <cstdio>
#include <string>
//===================================================================================
namespace Nelson {
//===================================================================================
static library_handle nlsGuiHandleDynamicLibrary = nullptr;
static bool bFirstDynamicLibraryCall = true;
//===================================================================================
static void
initGuiDynamicLibrary()
{
    if (bFirstDynamicLibraryCall) {
        std::string fullpathGuiSharedLibrary = "libnlsGui" + get_dynamic_library_extension();
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
        char const* tmp = std::getenv("NELSON_BINARY_PATH");
        if (tmp != nullptr) {
            fullpathGuiSharedLibrary
                = std::string(tmp) + std::string("/") + fullpathGuiSharedLibrary;
        }
#endif
        nlsGuiHandleDynamicLibrary = load_dynamic_library(fullpathGuiSharedLibrary);
        if (nlsGuiHandleDynamicLibrary != nullptr) {
            bFirstDynamicLibraryCall = false;
        } else {
            std::string msg = _("Gui module not loaded.") + std::string("\n");
            fprintf(stderr, "%s", msg.c_str());
            std::string error_msg = get_dynamic_library_error();
            fprintf(stderr, "%s", error_msg.c_str());
            exit(1);
        }
    }
}
//===================================================================================
void
InitGuiObjectsDynamic()
{
    using PROC_InitGuiObjects = void (*)();
    static PROC_InitGuiObjects InitGuiObjectsPtr = nullptr;
    initGuiDynamicLibrary();
    if (InitGuiObjectsPtr == nullptr) {
        InitGuiObjectsPtr = reinterpret_cast<PROC_InitGuiObjects>(
            get_function(nlsGuiHandleDynamicLibrary, "InitGuiObjects"));
        if (InitGuiObjectsPtr == nullptr) {
            std::string msg = _("Gui Function not loaded.") + std::string("\n");
            fprintf(stderr, "%s", msg.c_str());
            std::string error_msg = get_dynamic_library_error();
            fprintf(stderr, "%s", error_msg.c_str());
            exit(1);
        }
    }
    InitGuiObjectsPtr();
}
//===================================================================================
void*
CreateGuiEvaluatorDynamic(void* vcontext, NELSON_ENGINE_MODE _mode, bool minimizeWindow)
{
    using PROC_CreateGuiEvaluator = void* (*)(void*, NELSON_ENGINE_MODE, bool);
    static PROC_CreateGuiEvaluator CreateGuiEvaluatorPtr = nullptr;
    initGuiDynamicLibrary();
    if (CreateGuiEvaluatorPtr == nullptr) {
        CreateGuiEvaluatorPtr = reinterpret_cast<PROC_CreateGuiEvaluator>(
            get_function(nlsGuiHandleDynamicLibrary, "CreateGuiEvaluator"));
        if (CreateGuiEvaluatorPtr == nullptr) {
            std::string msg = _("Gui Function not loaded.") + std::string("\n");
            fprintf(stderr, "%s", msg.c_str());
            std::string error_msg = get_dynamic_library_error();
            fprintf(stderr, "%s", error_msg.c_str());
            exit(1);
        }
    }
    return CreateGuiEvaluatorPtr(vcontext, _mode, minimizeWindow);
}
//===================================================================================
void
DestroyMainGuiObjectDynamic(void* term)
{
    using PROC_DestroyMainGuiObject = void (*)(void*);
    static PROC_DestroyMainGuiObject DestroyMainGuiObjectPtr = nullptr;
    initGuiDynamicLibrary();
    if (DestroyMainGuiObjectPtr == nullptr) {
        DestroyMainGuiObjectPtr = reinterpret_cast<PROC_DestroyMainGuiObject>(
            get_function(nlsGuiHandleDynamicLibrary, "DestroyMainGuiObject"));
        if (DestroyMainGuiObjectPtr == nullptr) {
            std::string msg = _("Gui Function not loaded.") + std::string("\n");
            fprintf(stderr, "%s", msg.c_str());
            std::string error_msg = get_dynamic_library_error();
            fprintf(stderr, "%s", error_msg.c_str());
            exit(1);
        }
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
        GetMainGuiObjectPtr = reinterpret_cast<PROC_GetMainGuiObject>(
            get_function(nlsGuiHandleDynamicLibrary, "GetMainGuiObject"));
        if (GetMainGuiObjectPtr == nullptr) {
            std::string msg = _("Gui Function not loaded.") + std::string("\n");
            fprintf(stderr, "%s", msg.c_str());
            std::string error_msg = get_dynamic_library_error();
            fprintf(stderr, "%s", error_msg.c_str());
            exit(1);
        }
    }
    return GetMainGuiObjectPtr();
}
//===================================================================================
} // namespace Nelson
//===================================================================================
