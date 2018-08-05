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
static void initInterpreterDynamicLibrary(void) {
  if (bFirstDynamicLibraryCall) {
    std::string fullpathInterpreterSharedLibrary =
        "libnlsInterpreter" + Nelson::get_dynamic_library_extension();
#ifdef _MSC_VER
    char *buf;
    try {
      buf = new char[MAX_PATH];
    } catch (std::bad_alloc) {
      buf = nullptr;
    }
    if (buf) {
      DWORD dwRet =
          ::GetEnvironmentVariableA("NELSON_BINARY_PATH", buf, MAX_PATH);
      if (dwRet) {
        fullpathInterpreterSharedLibrary = std::string(buf) + std::string("/") +
                                           fullpathInterpreterSharedLibrary;
      }
      delete[] buf;
    }
#else
    char const *tmp = getenv("NELSON_BINARY_PATH");
    if (tmp != nullptr) {
      fullpathInterpreterSharedLibrary = std::string(tmp) + std::string("/") +
                                         fullpathInterpreterSharedLibrary;
    }
#endif
    nlsInterpreterHandleDynamicLibrary =
        Nelson::load_dynamic_library(fullpathInterpreterSharedLibrary);
    if (nlsInterpreterHandleDynamicLibrary) {
      bFirstDynamicLibraryCall = false;
    }
  }
}
//=============================================================================
static void NelsonWarningEmitterDynamicFunction(Exception *e, bool asError) {
  typedef void (*PROC_NelsonWarningEmitter)(void *, bool);
  static PROC_NelsonWarningEmitter NelsonWarningEmitterPtr = nullptr;
  initInterpreterDynamicLibrary();
  if (!NelsonWarningEmitterPtr) {
    NelsonWarningEmitterPtr =
        reinterpret_cast<PROC_NelsonWarningEmitter>(Nelson::get_function(
            nlsInterpreterHandleDynamicLibrary, "NelsonWarningEmitter"));
  }
  if (NelsonWarningEmitterPtr) {
    NelsonWarningEmitterPtr((void *)e, asError);
  }
}
//=============================================================================
void Warning(std::wstring id, std::wstring message) {
  if (message.compare(L"") != 0) {
    WARNING_STATE state = warningCheckState(message);
    switch (state) {
    case WARNING_STATE::AS_ERROR: {
      Exception warningException(message, L"", -1, -1, L"", id);
      NelsonWarningEmitterDynamicFunction(&warningException, true);
    } break;
    case WARNING_STATE::DISABLED:
      break;
    case WARNING_STATE::ENABLED:
    case WARNING_STATE::NOT_FOUND:
    default:
      Exception warningException(message, L"", -1, -1, L"", id);
      NelsonWarningEmitterDynamicFunction(&warningException, false);
      break;
    }
  }
}
//=============================================================================
void Warning(std::wstring message) { Warning(L"all", message); }
//=============================================================================
void Warning(std::string message) { Warning(L"all", utf8_to_wstring(message)); }
//=============================================================================
void Warning(std::string id, std::string message) {
  Warning(utf8_to_wstring(id), utf8_to_wstring(message));
}
//=============================================================================
} // namespace Nelson
//=============================================================================
