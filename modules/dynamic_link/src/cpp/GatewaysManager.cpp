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
#include <boost/unordered_map.hpp>
#include "GatewaysManager.hpp"
#include "characters_encoding.hpp"
#include "FindDynamicLibraryName.hpp"
#include "dynamic_library.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#define GATEWAY_ENTRY "AddGateway"
#define REMOVEGATEWAY_ENTRY "RemoveGateway"
//=============================================================================
boost::unordered_map<std::wstring, library_handle> libraryMap;
//=============================================================================
GatewaysManager* GatewaysManager::m_pInstance = nullptr;
//=============================================================================
GatewaysManager::GatewaysManager() { libraryMap.reserve(128); }
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
GatewaysManager::destroy()
{
    if (m_pInstance != nullptr) {
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
        library_handle nlsModuleHandleDynamicLibrary = nullptr;
        boost::unordered_map<std::wstring, library_handle>::iterator found
            = libraryMap.find(libraryFullName);
        if (found != libraryMap.end()) {
            nlsModuleHandleDynamicLibrary = found->second;
        } else {
#ifdef _MSC_VER
            nlsModuleHandleDynamicLibrary = load_dynamic_libraryW(filename);
#else
            nlsModuleHandleDynamicLibrary = load_dynamic_library(wstring_to_utf8(filename));
#endif
            needToAdd = true;
        }

        if (nlsModuleHandleDynamicLibrary) {
            using PROC_AddGateway = bool (*)(const void*, const wchar_t*);
            PROC_AddGateway AddGatewayPtr = reinterpret_cast<PROC_AddGateway>(
                get_function(nlsModuleHandleDynamicLibrary, GATEWAY_ENTRY));
            boost::filesystem::current_path(currentdirbackup);
            if (AddGatewayPtr) {
                if (needToAdd) {
                    libraryMap.emplace(libraryFullName, nlsModuleHandleDynamicLibrary);
                }
                return AddGatewayPtr((void*)eval, libraryFullName.c_str());
            } else {
                errorMessage = _W("Module not loaded: symbol not found.");
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

    boost::unordered_map<std::wstring, library_handle>::iterator found
        = libraryMap.find(libraryFullName);
    if (found != libraryMap.end()) {
        library_handle nlsModuleHandleDynamicLibrary = found->second;
        if (nlsModuleHandleDynamicLibrary) {
            using PROC_RemoveGateway = bool (*)(void*, const wchar_t*);
            PROC_RemoveGateway RemoveGatewayPtr = reinterpret_cast<PROC_RemoveGateway>(
                get_function(nlsModuleHandleDynamicLibrary, REMOVEGATEWAY_ENTRY));
            boost::filesystem::current_path(currentdirbackup);
            if (RemoveGatewayPtr) {
                bool res = RemoveGatewayPtr((void*)eval, libraryFullName.c_str());
                close_dynamic_library(nlsModuleHandleDynamicLibrary);
                libraryMap.erase(libraryFullName);
                return res;
            } else {
                errorMessage = _W("Module not loaded: symbol not found.");
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
}
//=============================================================================
