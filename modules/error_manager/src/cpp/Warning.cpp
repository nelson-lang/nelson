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
#include "Warning.hpp"
#include "Error.hpp"
#include "characters_encoding.hpp"
#include "DynamicLibrary.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static Nelson::library_handle nlsInterpreterHandleDynamicLibrary = nullptr;
static bool bFirstDynamicLibraryCall = true;
//=============================================================================
static void
initInterpreterDynamicLibrary()
{
    if (bFirstDynamicLibraryCall) {
        std::wstring fullpathInterpreterSharedLibrary
            = L"libnlsInterpreter" + Nelson::get_dynamic_library_extensionW();
        std::wstring nelsonLibrariesDirectory
            = NelsonConfiguration::getInstance()->getNelsonLibraryDirectory();
        fullpathInterpreterSharedLibrary
            = nelsonLibrariesDirectory + std::wstring(L"/") + fullpathInterpreterSharedLibrary;
        nlsInterpreterHandleDynamicLibrary
            = Nelson::load_dynamic_libraryW(fullpathInterpreterSharedLibrary);
        if (nlsInterpreterHandleDynamicLibrary != nullptr) {
            bFirstDynamicLibraryCall = false;
        }
    }
}
//=============================================================================
static void
NelsonWarningEmitterDynamicFunction(const std::wstring& msg, const std::wstring& id, bool asError)
{
    using PROC_NelsonWarningEmitter = void (*)(const wchar_t*, const wchar_t*, bool);
    static PROC_NelsonWarningEmitter NelsonWarningEmitterPtr = nullptr;
    initInterpreterDynamicLibrary();
    if (NelsonWarningEmitterPtr == nullptr) {
        NelsonWarningEmitterPtr = reinterpret_cast<PROC_NelsonWarningEmitter>(
            Nelson::get_function(nlsInterpreterHandleDynamicLibrary, "NelsonWarningEmitter"));
    }
    if (NelsonWarningEmitterPtr != nullptr) {
        NelsonWarningEmitterPtr(msg.c_str(), id.c_str(), asError);
    }
}
//=============================================================================
void
Warning(const std::wstring& id, const std::wstring& message)
{
    if (message != L"") {
        WARNING_STATE state = warningCheckState(id);
        switch (state) {
        case WARNING_STATE::AS_ERROR: {
            NelsonWarningEmitterDynamicFunction(message, id, true);
        } break;
        case WARNING_STATE::DISABLED:
            break;
        case WARNING_STATE::ENABLED:
        case WARNING_STATE::NOT_FOUND:
        default: {
            NelsonWarningEmitterDynamicFunction(message, id, false);
        } break;
        }
    }
}
//=============================================================================
void
Warning(const std::wstring& message)
{
    Warning(L"", message);
}
//=============================================================================
void
Warning(const std::string& message)
{
    Warning(L"", utf8_to_wstring(message));
}
//=============================================================================
void
Warning(const std::string& id, const std::string& message)
{
    Warning(utf8_to_wstring(id), utf8_to_wstring(message));
}
//=============================================================================
} // namespace Nelson
//=============================================================================
