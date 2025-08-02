//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cstdlib>
#include "PostCommandDynamicFunction.hpp"
#include "DynamicLibrary.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
static Nelson::library_handle nlsEngineHandleDynamicLibrary = nullptr;
static bool bFirstDynamicLibraryCall = true;
//=============================================================================
static void
initEngineDynamicLibrary()
{
    if (bFirstDynamicLibraryCall) {
        std::wstring fullpathEngineSharedLibrary
            = L"libnlsEngine" + Nelson::get_dynamic_library_extensionW();
        std::wstring nelsonLibrariesDirectory
            = Nelson::NelsonConfiguration::getInstance()->getNelsonLibraryDirectory();
        fullpathEngineSharedLibrary
            = nelsonLibrariesDirectory + std::wstring(L"/") + fullpathEngineSharedLibrary;
        nlsEngineHandleDynamicLibrary = Nelson::load_dynamic_libraryW(fullpathEngineSharedLibrary);
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
