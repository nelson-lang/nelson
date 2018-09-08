//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
initGuiDynamicLibrary(void)
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
        if (buf) {
            DWORD dwRet = ::GetEnvironmentVariableA("NELSON_BINARY_PATH", buf, MAX_PATH);
            if (dwRet) {
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
        if (nlsGuiHandleDynamicLibrary) {
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
InitGuiObjectsDynamic(void)
{
    typedef void (*PROC_InitGuiObjects)(void);
    static PROC_InitGuiObjects InitGuiObjectsPtr = nullptr;
    initGuiDynamicLibrary();
    if (!InitGuiObjectsPtr) {
        InitGuiObjectsPtr = reinterpret_cast<PROC_InitGuiObjects>(
            get_function(nlsGuiHandleDynamicLibrary, "InitGuiObjects"));
        if (!InitGuiObjectsPtr) {
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
CreateGuiEvaluatorDynamic(void* vcontext, NELSON_ENGINE_MODE _mode)
{
    typedef void* (*PROC_CreateGuiEvaluator)(void* vcontext, NELSON_ENGINE_MODE _mode);
    static PROC_CreateGuiEvaluator CreateGuiEvaluatorPtr = nullptr;
    initGuiDynamicLibrary();
    if (!CreateGuiEvaluatorPtr) {
        CreateGuiEvaluatorPtr = reinterpret_cast<PROC_CreateGuiEvaluator>(
            get_function(nlsGuiHandleDynamicLibrary, "CreateGuiEvaluator"));
        if (!CreateGuiEvaluatorPtr) {
            std::string msg = _("Gui Function not loaded.") + std::string("\n");
            fprintf(stderr, "%s", msg.c_str());
            std::string error_msg = get_dynamic_library_error();
            fprintf(stderr, "%s", error_msg.c_str());
            exit(1);
        }
    }
    return CreateGuiEvaluatorPtr(vcontext, _mode);
}
//===================================================================================
void
DestroyMainGuiObjectDynamic(void* term)
{
    typedef void (*PROC_DestroyMainGuiObject)(void*);
    static PROC_DestroyMainGuiObject DestroyMainGuiObjectPtr = nullptr;
    initGuiDynamicLibrary();
    if (!DestroyMainGuiObjectPtr) {
        DestroyMainGuiObjectPtr = reinterpret_cast<PROC_DestroyMainGuiObject>(
            get_function(nlsGuiHandleDynamicLibrary, "DestroyMainGuiObject"));
        if (!DestroyMainGuiObjectPtr) {
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
GetMainGuiObjectDynamic(void)
{
    typedef void* (*PROC_GetMainGuiObject)(void);
    static PROC_GetMainGuiObject GetMainGuiObjectPtr = nullptr;
    initGuiDynamicLibrary();
    if (!GetMainGuiObjectPtr) {
        GetMainGuiObjectPtr = reinterpret_cast<PROC_GetMainGuiObject>(
            get_function(nlsGuiHandleDynamicLibrary, "GetMainGuiObject"));
        if (!GetMainGuiObjectPtr) {
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
}
//===================================================================================
