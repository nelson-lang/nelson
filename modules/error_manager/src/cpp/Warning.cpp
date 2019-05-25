//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
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
#include <cstdlib>
#include "Warning.hpp"
#include "Error.hpp"
#include "WarningIds.hpp"
#include "characters_encoding.hpp"
#include "dynamic_library.hpp"
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
