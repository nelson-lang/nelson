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
#include <map>
#include <utility>
#include <boost/filesystem.hpp>
#include <boost/dll/library_info.hpp>
#include "GatewaysManager.hpp"
#include "characters_encoding.hpp"
#include "FindDynamicLibraryName.hpp"
#include "dynamic_library.hpp"
#include "BuiltInFunctionDefManager.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#define GATEWAY_ENTRY "AddGateway"
#define REMOVEGATEWAY_ENTRY "RemoveGateway"
#define MEXCLEARATEXIT_ENTRY "mexClearAtExit"
#define MEXFUNCTIONNAME_ENTRY "mexFunctionName"
#define MEXFUNCTION_ENTRY "mexFunction"
#define MEXFILEREQUIREDAPIVERSION_ENTRY "mexfilerequiredapiversion"
//=============================================================================
static std::map<std::wstring, std::pair<library_handle, bool>> libraryMap;
//=============================================================================
GatewaysManager* GatewaysManager::m_pInstance = nullptr;
//=============================================================================
GatewaysManager::GatewaysManager() {}
//=============================================================================
GatewaysManager*
GatewaysManager::getInstance()
{
    if (m_pInstance == nullptr) {
        try {
            m_pInstance = new GatewaysManager();
        } catch (const std::bad_alloc&) {
            m_pInstance = nullptr;
        }
    }
    return m_pInstance;
}
//=============================================================================
void
GatewaysManager::destroy(Evaluator* eval)
{
    if (m_pInstance != nullptr) {
        wstringVector names;
        for (auto it = libraryMap.rbegin(); it != libraryMap.rend(); ++it) {
            names.push_back(it->first);
        }
        for (auto n : names) {
            std::wstring errorMessage;
            removeGateway(eval, n, errorMessage);
        }
        delete m_pInstance;
        m_pInstance = nullptr;
    }
}
//=============================================================================
bool
GatewaysManager::addGateway(
    Evaluator* eval, const std::wstring& libraryFullName, std::wstring& errorMessage)
{
    /* to simplify some dependencies resolution, we move in the directory and restore it after */
    boost::filesystem::path p = libraryFullName;
    p = p.generic_wstring();
    std::wstring filename;
    boost::filesystem::path dir = p.parent_path();
    if (dir.generic_wstring().compare(L"") == 0) {
        dir = boost::filesystem::current_path();
    }
    const std::wstring dirname = dir.generic_wstring();
    filename = p.filename().generic_wstring();
    filename = FindDynamicLibraryName(dirname, filename, false);
    if (filename.empty()) {
        errorMessage = _W("File not found.");
        return false;
    } else {
        boost::filesystem::path currentdirbackup = boost::filesystem::current_path();
        boost::filesystem::current_path(dir);

        bool needToAdd = false;
        bool isMex = false;
        library_handle nlsModuleHandleDynamicLibrary = nullptr;
        std::map<std::wstring, std::pair<library_handle, bool>>::iterator found
            = libraryMap.find(libraryFullName);
        if (found != libraryMap.end()) {
            nlsModuleHandleDynamicLibrary = found->second.first;
        } else {
#ifdef _MSC_VER
            nlsModuleHandleDynamicLibrary = load_dynamic_libraryW(filename);
#else
            nlsModuleHandleDynamicLibrary = load_dynamic_library(wstring_to_utf8(filename));
#endif
            needToAdd = true;
        }

        if (nlsModuleHandleDynamicLibrary != nullptr) {
            using PROC_MexFileRequiredApiVersion = void (*)(int*, int*);
            using PROC_mexFunctionName = const char* (*)(void);
            using PROC_AddGateway = bool (*)(const void*, const wchar_t*);
            generic_function_ptr PROC_mexFunctionPtr = nullptr;
            PROC_mexFunctionName PROC_mexFunctionNamePtr = nullptr;
            PROC_MexFileRequiredApiVersion PROC_MexFileRequiredApiVersionPtr = nullptr;

            PROC_AddGateway AddGatewayPtr = reinterpret_cast<PROC_AddGateway>(
                get_function(nlsModuleHandleDynamicLibrary, GATEWAY_ENTRY));
            if (AddGatewayPtr) {
                isMex = false;
            } else {
                PROC_MexFileRequiredApiVersionPtr
                    = reinterpret_cast<PROC_MexFileRequiredApiVersion>(get_function(
                        nlsModuleHandleDynamicLibrary, MEXFILEREQUIREDAPIVERSION_ENTRY));
                PROC_mexFunctionNamePtr = reinterpret_cast<PROC_mexFunctionName>(
                    get_function(nlsModuleHandleDynamicLibrary, MEXFUNCTIONNAME_ENTRY));
                PROC_mexFunctionPtr
                    = get_function(nlsModuleHandleDynamicLibrary, MEXFUNCTION_ENTRY);
                isMex = PROC_mexFunctionNamePtr != nullptr && PROC_mexFunctionPtr != nullptr
                    && PROC_MexFileRequiredApiVersionPtr != nullptr;
                if (!isMex) {
                    boost::filesystem::current_path(currentdirbackup);
                    errorMessage = _W("Module not loaded: symbol not found.");
                    return false;
                }
            }
            if (needToAdd) {
                std::pair<library_handle, bool> pairHandleIsMex(
                    nlsModuleHandleDynamicLibrary, isMex);
                libraryMap.emplace(libraryFullName, pairHandleIsMex);
            }
            boost::filesystem::current_path(currentdirbackup);
            if (isMex) {
                int buildRelease = 0;
                int targetApiVersion = 0;
                PROC_MexFileRequiredApiVersionPtr(&buildRelease, &targetApiVersion);
                bool interleavedComplex = targetApiVersion != 0x07300000;
                std::string mexFunctionName = PROC_mexFunctionNamePtr();
                return Nelson::BuiltInFunctionDefManager::getInstance()->add(mexFunctionName,
                    PROC_mexFunctionPtr, -1, -1, libraryFullName, utf8_to_wstring(mexFunctionName),
                    C_MEX_BUILTIN, interleavedComplex);
            } else {
                return AddGatewayPtr((void*)eval, libraryFullName.c_str());
            }
        } else {
            std::string error_msg = get_dynamic_library_error();
            boost::filesystem::current_path(currentdirbackup);
            errorMessage = _W("Module not loaded: library not loaded.\n") + libraryFullName + L"\n"
                + utf8_to_wstring(error_msg) + L"\n";
        }
    }
    return false;
}
//=============================================================================
bool
GatewaysManager::removeGateway(
    Evaluator* eval, const std::wstring& libraryFullName, std::wstring& errorMessage)
{
    /* to simplify some dependencies resolution, we move in the directory and restore it after */
    boost::filesystem::path p;
    p = libraryFullName;
    p = p.generic_wstring();
    std::wstring dirname;
    std::wstring filename;
    boost::filesystem::path dir = p.parent_path();
    if (dir.generic_wstring().compare(L"") == 0) {
        dir = boost::filesystem::current_path();
    }
    dirname = dir.generic_wstring();
    filename = p.filename().generic_wstring();
    filename = FindDynamicLibraryName(dirname, filename, false);
    if (filename.empty()) {
        errorMessage = _W("File not found.");
        return false;
    }
    boost::filesystem::path currentdirbackup = boost::filesystem::current_path();
    boost::filesystem::current_path(dir);

    std::map<std::wstring, std::pair<library_handle, bool>>::iterator found
        = libraryMap.find(libraryFullName);
    if (found != libraryMap.end()) {
        library_handle nlsModuleHandleDynamicLibrary = found->second.first;
        bool isMex = found->second.second;
        if (nlsModuleHandleDynamicLibrary) {
            if (isMex) {
                clearMexGateway(libraryFullName);
                boost::filesystem::current_path(currentdirbackup);
                libraryMap.erase(libraryFullName);
                close_dynamic_library(nlsModuleHandleDynamicLibrary);
            } else {
                using PROC_RemoveGateway = bool (*)(void*, const wchar_t*);
                PROC_RemoveGateway RemoveGatewayPtr = reinterpret_cast<PROC_RemoveGateway>(
                    get_function(nlsModuleHandleDynamicLibrary, REMOVEGATEWAY_ENTRY));
                boost::filesystem::current_path(currentdirbackup);
                if (RemoveGatewayPtr) {
                    bool res = RemoveGatewayPtr((void*)eval, libraryFullName.c_str());
                    libraryMap.erase(libraryFullName);
                    close_dynamic_library(nlsModuleHandleDynamicLibrary);
                    return res;
                } else {
                    errorMessage = _W("Module not loaded: symbol not found.");
                }
            }
        } else {
            errorMessage = _W("Module not loaded: library handle not found.");
        }
    } else {
        errorMessage = _W("Module not loaded: library handle not found.");
    }
    return false;
}
//=============================================================================
wstringVector
GatewaysManager::getLibraryNames()
{
    wstringVector names;
    for (auto it = libraryMap.rbegin(); it != libraryMap.rend(); ++it) {
        names.push_back(it->first);
    }
    return names;
}
//=============================================================================
bool
GatewaysManager::clearMexGateway(const std::wstring& libraryFullName)
{
    std::map<std::wstring, std::pair<library_handle, bool>>::iterator found
        = libraryMap.find(libraryFullName);
    if (found != libraryMap.end()) {
        library_handle nlsModuleHandleDynamicLibrary = found->second.first;
        using PROC_ClearMex = void (*)(void);
        PROC_ClearMex ClearMexPtr = reinterpret_cast<PROC_ClearMex>(
            get_function(nlsModuleHandleDynamicLibrary, MEXCLEARATEXIT_ENTRY));
        if (ClearMexPtr) {
            ClearMexPtr();
            return true;
        }
    }
    return false;
}
//=============================================================================
}
//=============================================================================
