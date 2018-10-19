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
#include <stdlib.h>
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
initInterpreterDynamicLibrary(void)
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
        if (buf) {
            DWORD dwRet = ::GetEnvironmentVariableA("NELSON_BINARY_PATH", buf, MAX_PATH);
            if (dwRet) {
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
        if (nlsInterpreterHandleDynamicLibrary) {
            bFirstDynamicLibraryCall = false;
        }
    }
}
//=============================================================================
void
Error(const std::wstring& msg, const std::wstring& id)
{
    typedef void (*PROC_NelsonErrorEmitter)(const wchar_t*, const wchar_t*);
    static PROC_NelsonErrorEmitter NelsonErrorEmitterPtr = nullptr;
    initInterpreterDynamicLibrary();
    if (!NelsonErrorEmitterPtr) {
        NelsonErrorEmitterPtr = reinterpret_cast<PROC_NelsonErrorEmitter>(
            Nelson::get_function(nlsInterpreterHandleDynamicLibrary, "NelsonErrorEmitter"));
    }
    if (NelsonErrorEmitterPtr) {
        NelsonErrorEmitterPtr(msg.c_str(), id.c_str());
    }
}
//=============================================================================
void
Error(const std::string& msg, const std::string& id)
{
    Error(utf8_to_wstring(msg), utf8_to_wstring(id));
}
//=============================================================================
}
//=============================================================================
