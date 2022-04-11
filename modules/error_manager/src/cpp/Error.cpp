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
#include "Error.hpp"
#include "characters_encoding.hpp"
#include "Exception.hpp"
#include "dynamic_library.hpp"
//=============================================================================
namespace Nelson {
//============================================================================
static Nelson::library_handle nlsInterpreterHandleDynamicLibrary = nullptr;
static bool bFirstDynamicLibraryCall = true;
//============================================================================
static void
initInterpreterDynamicLibrary()
{
    if (bFirstDynamicLibraryCall) {
        std::string fullpathInterpreterSharedLibrary
            = "libnlsInterpreter" + Nelson::get_dynamic_library_extension();
#ifdef _MSC_VER
        char* buf;
        try {
            buf = new char[MAX_PATH];
        } catch (const std::bad_alloc&) {
            buf = nullptr;
        }
        if (buf != nullptr) {
            DWORD dwRet = ::GetEnvironmentVariableA("NELSON_BINARY_PATH", buf, MAX_PATH);
            if (dwRet != 0u) {
                fullpathInterpreterSharedLibrary
                    = std::string(buf) + std::string("/") + fullpathInterpreterSharedLibrary;
            }
            delete[] buf;
        }
#else
        char const* tmp = getenv("NELSON_BINARY_PATH");
        if (tmp != nullptr) {
            fullpathInterpreterSharedLibrary
                = std::string(tmp) + std::string("/") + fullpathInterpreterSharedLibrary;
        }
#endif
        nlsInterpreterHandleDynamicLibrary
            = Nelson::load_dynamic_library(fullpathInterpreterSharedLibrary);
        if (nlsInterpreterHandleDynamicLibrary != nullptr) {
            bFirstDynamicLibraryCall = false;
        }
    }
}
//=============================================================================
void
Error(const std::wstring& msg, const std::wstring& id, bool asCaller)
{
    using PROC_NelsonErrorEmitter = void (*)(const wchar_t*, const wchar_t*, bool);
    static PROC_NelsonErrorEmitter NelsonErrorEmitterPtr = nullptr;
    initInterpreterDynamicLibrary();
    if (NelsonErrorEmitterPtr == nullptr) {
        NelsonErrorEmitterPtr = reinterpret_cast<PROC_NelsonErrorEmitter>(
            Nelson::get_function(nlsInterpreterHandleDynamicLibrary, "NelsonErrorEmitter"));
    }
    if (NelsonErrorEmitterPtr != nullptr) {
        NelsonErrorEmitterPtr(msg.c_str(), id.c_str(), asCaller);
    }
}
//=============================================================================
void
Error(const std::string& msg, const std::string& id, bool asCaller)
{
    Error(utf8_to_wstring(msg), utf8_to_wstring(id), asCaller);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
