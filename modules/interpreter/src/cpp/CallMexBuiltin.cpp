//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "CallMexBuiltin.hpp"
#include "dynamic_library.hpp"
//=============================================================================
static Nelson::library_handle nlsMexHandleDynamicLibrary = nullptr;
static bool bFirstDynamicLibraryCall = true;
//=============================================================================
static void
initMexDynamicLibrary()
{
    if (bFirstDynamicLibraryCall) {
        std::string fullpathMexSharedLibrary
            = "libnlsMex" + Nelson::get_dynamic_library_extension();
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
                fullpathMexSharedLibrary
                    = std::string(buf) + std::string("/") + fullpathMexSharedLibrary;
            }
            delete[] buf;
        }
#else
        char const* tmp = getenv("NELSON_BINARY_PATH");
        if (tmp != nullptr) {
            fullpathMexSharedLibrary
                = std::string(tmp) + std::string("/") + fullpathMexSharedLibrary;
        }
#endif
        nlsMexHandleDynamicLibrary = Nelson::load_dynamic_library(fullpathMexSharedLibrary);
        if (nlsMexHandleDynamicLibrary != nullptr) {
            bFirstDynamicLibraryCall = false;
        }
    }
}
//=============================================================================
namespace Nelson {
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
