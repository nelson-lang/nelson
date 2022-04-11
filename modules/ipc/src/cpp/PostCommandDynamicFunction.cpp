//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cstdlib>
#include "PostCommandDynamicFunction.hpp"
#include "dynamic_library.hpp"
//=============================================================================
static Nelson::library_handle nlsEngineHandleDynamicLibrary = nullptr;
static bool bFirstDynamicLibraryCall = true;
//=============================================================================
static void
initEngineDynamicLibrary()
{
    if (bFirstDynamicLibraryCall) {
        std::string fullpathEngineSharedLibrary
            = "libnlsEngine" + Nelson::get_dynamic_library_extension();
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
                fullpathEngineSharedLibrary
                    = std::string(buf) + std::string("/") + fullpathEngineSharedLibrary;
            }
            delete[] buf;
        }
#else
        char const* tmp = getenv("NELSON_BINARY_PATH");
        if (tmp != nullptr) {
            fullpathEngineSharedLibrary
                = std::string(tmp) + std::string("/") + fullpathEngineSharedLibrary;
        }
#endif
        nlsEngineHandleDynamicLibrary = Nelson::load_dynamic_library(fullpathEngineSharedLibrary);
        if (nlsEngineHandleDynamicLibrary != nullptr) {
            bFirstDynamicLibraryCall = false;
        }
    }
}
//=============================================================================
bool
PostCommandDynamicFunction(const std::wstring& command)
{
    using PROC_PostCommand = bool (*)(const std::wstring&);
    static PROC_PostCommand PostCommandPtr = nullptr;
    initEngineDynamicLibrary();
    if (PostCommandPtr == nullptr) {
        PostCommandPtr = reinterpret_cast<PROC_PostCommand>(
            Nelson::get_function(nlsEngineHandleDynamicLibrary, "PostCommand"));
        if (PostCommandPtr == nullptr) {
            return false;
        }
    }
    return PostCommandPtr(command);
}
//=============================================================================
