//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "CallMexBuiltin.hpp"
#include "DynamicLibrary.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
static Nelson::library_handle nlsMexHandleDynamicLibrary = nullptr;
static bool bFirstDynamicLibraryCall = true;
//=============================================================================
namespace Nelson {
//=============================================================================
static void
initMexDynamicLibrary()
{
    if (bFirstDynamicLibraryCall) {
        std::wstring fullpathMexSharedLibrary
            = L"libnlsMex" + Nelson::get_dynamic_library_extensionW();
        std::wstring nelsonLibrariesDirectory
            = NelsonConfiguration::getInstance()->getNelsonLibraryDirectory();
        fullpathMexSharedLibrary
            = nelsonLibrariesDirectory + std::wstring(L"/") + fullpathMexSharedLibrary;
        nlsMexHandleDynamicLibrary = Nelson::load_dynamic_libraryW(fullpathMexSharedLibrary);
        if (nlsMexHandleDynamicLibrary != nullptr) {
            bFirstDynamicLibraryCall = false;
        }
    }
}
//=============================================================================
void
CallMexBuiltin(void* fptr, const ArrayOfVector& inputArgs, int nargout, ArrayOfVector& outputArgs,
    bool interleavedComplex)
{
    using PROC_mxCallBuiltin = void (*)(void* fptr, const ArrayOfVector& inputArgs, int nargout,
        ArrayOfVector& outputArgs, bool interleavedComplex);
    static PROC_mxCallBuiltin mxCallBuiltinPtr = nullptr;
    initMexDynamicLibrary();
    if (mxCallBuiltinPtr == nullptr) {
        mxCallBuiltinPtr = reinterpret_cast<PROC_mxCallBuiltin>(
            Nelson::get_function(nlsMexHandleDynamicLibrary, "mxCallBuiltin"));
    }
    if (mxCallBuiltinPtr != nullptr) {
        mxCallBuiltinPtr(fptr, inputArgs, nargout, outputArgs, interleavedComplex);
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
