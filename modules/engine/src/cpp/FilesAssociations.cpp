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
#include <boost/filesystem.hpp>
#include <cstdlib>
#include <string>
#include "dynamic_library.hpp"
#include "FilesAssociation.hpp"
#include "EvaluateCommand.hpp"
#include "NelSon_engine_mode.h"
//=============================================================================
static Nelson::library_handle nlsGuiHandleDynamicLibrary = nullptr;
static bool bFirstDynamicLibraryCall = true;
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
postCommandInGuiThreadDynamicFunction(const std::wstring& commandToExecute);
//=============================================================================
static void
initGuiDynamicLibrary();
//=============================================================================
static bool
commonFilesAssociated(
    NELSON_ENGINE_MODE currentMode, const std::wstring& command, const wstringVector& filesToOpen);
//=============================================================================
bool
OpenFilesAssociated(NELSON_ENGINE_MODE currentMode, const wstringVector& filesToOpen)
{
    return commonFilesAssociated(currentMode, L"edit", filesToOpen);
}
//=============================================================================
bool
LoadFilesAssociated(NELSON_ENGINE_MODE currentMode, const wstringVector& filesToOpen)
{
    return commonFilesAssociated(currentMode, L"load", filesToOpen);
}
//=============================================================================
bool
ExecuteFilesAssociated(NELSON_ENGINE_MODE currentMode, const wstringVector& filesToOpen)
{
    return commonFilesAssociated(currentMode, L"run", filesToOpen);
}
//=============================================================================
bool
commonFilesAssociated(
    NELSON_ENGINE_MODE currentMode, const std::wstring& command, const wstringVector& filesToOpen)
{
    bool res = false;
    if (currentMode == NELSON_ENGINE_MODE::GUI) {
        if (!filesToOpen.empty()) {
            try {
                for (size_t k = 0; k < filesToOpen.size(); k++) {
                    boost::filesystem::path pathFileToOpen(filesToOpen[k]);
                    bool bIsFile = boost::filesystem::exists(pathFileToOpen)
                        && !boost::filesystem::is_directory(pathFileToOpen);
                    if (bIsFile) {
                        std::wstring commandToExecute
                            = command + std::wstring(L"('" + filesToOpen[k] + L"');");
                        bool r = postCommandInGuiThreadDynamicFunction(commandToExecute);
                        if (r != true) {
                            return false;
                        }
                    } else {
                        return false;
                    }
                }
                return true;
            } catch (Exception&) {
                res = false;
            }
        }
    }
    return res;
}
//=============================================================================
bool
postCommandInGuiThreadDynamicFunction(const std::wstring& commandToExecute)
{
    using PROC_postCommandToNelson = bool (*)(const std::wstring& commandToExecute);
    static PROC_postCommandToNelson postCommandToNelsonPtr = nullptr;
    initGuiDynamicLibrary();
    if (postCommandToNelsonPtr == nullptr) {
        postCommandToNelsonPtr = reinterpret_cast<PROC_postCommandToNelson>(
            Nelson::get_function(nlsGuiHandleDynamicLibrary, "postCommandToNelson"));
        if (postCommandToNelsonPtr == nullptr) {
            return false;
        }
    }
    return postCommandToNelsonPtr(commandToExecute);
}
//=============================================================================
void
initGuiDynamicLibrary()
{
    if (bFirstDynamicLibraryCall) {
        std::string fullpathGuiSharedLibrary
            = "libnlsGui" + Nelson::get_dynamic_library_extension();
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
                fullpathGuiSharedLibrary
                    = std::string(buf) + std::string("/") + fullpathGuiSharedLibrary;
            }
            delete[] buf;
        }
#else
        char const* tmp = getenv("NELSON_BINARY_PATH");
        if (tmp != nullptr) {
            fullpathGuiSharedLibrary
                = std::string(tmp) + std::string("/") + fullpathGuiSharedLibrary;
        }
#endif
        nlsGuiHandleDynamicLibrary = Nelson::load_dynamic_library(fullpathGuiSharedLibrary);
        if (nlsGuiHandleDynamicLibrary != nullptr) {
            bFirstDynamicLibraryCall = false;
        }
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
