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
    using PROC_PostCommand = bool (*)(const std::wstring &);
    static PROC_PostCommand PostCommandPtr = nullptr;
    initEngineDynamicLibrary();
    if (PostCommandPtr == nullptr) {
        PostCommandPtr = reinterpret_cast<PROC_PostCommand>(
            Nelson::get_function(nlsEngineHandleDynamicLibrary, "PostCommand"));
        if (PostCommandPtr == nullptr) {
            return nullptr;
        }
    }
    return PostCommandPtr(command);
}
//=============================================================================
