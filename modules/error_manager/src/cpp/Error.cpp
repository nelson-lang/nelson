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
#include "DynamicLibrary.hpp"
#include "NelsonConfiguration.hpp"
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
        std::wstring fullpathInterpreterSharedLibrary
            = L"libnlsInterpreter" + Nelson::get_dynamic_library_extensionW();
        std::wstring nelsonBinaryDirectory
            = NelsonConfiguration::getInstance()->getNelsonBinaryDirectory();
        fullpathInterpreterSharedLibrary
            = nelsonBinaryDirectory + std::wstring(L"/") + fullpathInterpreterSharedLibrary;
        nlsInterpreterHandleDynamicLibrary
            = Nelson::load_dynamic_libraryW(fullpathInterpreterSharedLibrary);
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
