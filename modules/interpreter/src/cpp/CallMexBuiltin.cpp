//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
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
CallMexBuiltin(void* fptr, const std::string& functionName, const ArrayOfVector& inputArgs,
    int nargout, ArrayOfVector& outputArgs)
{
    using PROC_mxCallBuiltin = void (*)(void* fptr, const std::string& functionName, const ArrayOfVector& inputArgs, int nargout, ArrayOfVector&outputArgs);
    static PROC_mxCallBuiltin mxCallBuiltinPtr = nullptr;
    initMexDynamicLibrary();
    if (mxCallBuiltinPtr == nullptr) {
        mxCallBuiltinPtr = reinterpret_cast<PROC_mxCallBuiltin>(
            Nelson::get_function(nlsMexHandleDynamicLibrary, "mxCallBuiltin"));
    }
    if (mxCallBuiltinPtr != nullptr) {
        mxCallBuiltinPtr(fptr, functionName, inputArgs, nargout, outputArgs);
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
