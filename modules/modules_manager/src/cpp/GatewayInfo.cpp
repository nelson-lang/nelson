//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FileSystemHelpers.hpp"
#include "GatewayInfo.hpp"
#include "Error.hpp"
#include "FindDynamicLibraryName.hpp"
#include "characters_encoding.hpp"
#include "dynamic_library.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
GatewayInfo(const std::wstring& dynlibname, std::wstring& moduleName, stringVector& functionsList,
    std::wstring& errorMessage)
{
    bool bRes = true;
    errorMessage.clear();
    moduleName.clear();
    functionsList.clear();
    /* to simplify some dependencies resolution, we move in the directory and restore it after */
    std::filesystem::path p;
    p = dynlibname;
    p = p.generic_string();
    std::wstring dirname;
    std::wstring filename;
    std::filesystem::path dir = p.parent_path();
    if (convertFileSytemPathToGenericWString(dir).compare(L"") == 0) {
        dir = std::filesystem::current_path();
    }
    dirname = convertFileSytemPathToGenericWString(dir);
    filename = convertFileSytemPathToGenericWString(p.filename());
    filename = FindDynamicLibraryName(dirname, filename, false);
    if (filename.empty()) {
        errorMessage = _W("File not found.");
        return false;
    }
    std::filesystem::path currentdirbackup = std::filesystem::current_path();
    std::filesystem::current_path(dir);
    library_handle nlsModuleHandleDynamicLibrary = nullptr;
#ifdef _MSC_VER
    nlsModuleHandleDynamicLibrary = load_dynamic_libraryW(filename);
#else
    nlsModuleHandleDynamicLibrary = load_dynamic_library(wstring_to_utf8(filename));
#endif
    if (nlsModuleHandleDynamicLibrary) {
        using PROC_InfoModule = stringVector (*)();
        using PROC_InfoModuleName = const wchar_t* (*)();
        PROC_InfoModule InfoModulePtr = reinterpret_cast<PROC_InfoModule>(
            get_function(nlsModuleHandleDynamicLibrary, GATEWAY_INFO));
        PROC_InfoModuleName InfoModuleNamePtr = reinterpret_cast<PROC_InfoModuleName>(
            get_function(nlsModuleHandleDynamicLibrary, GATEWAY_NAME));
        std::filesystem::current_path(currentdirbackup);
        if (!InfoModulePtr) {
            errorMessage = _W("Module not loaded: symbol not found.");
            std::filesystem::current_path(currentdirbackup);
            return false;
        }
        functionsList = InfoModulePtr();
        if (InfoModuleNamePtr) {
            moduleName = InfoModuleNamePtr();
        } else {
            errorMessage = _W("Module not loaded: symbol not found.");
            std::filesystem::current_path(currentdirbackup);
            return false;
        }
    } else {
        std::filesystem::current_path(currentdirbackup);
        errorMessage = _W("Module not loaded: library not loaded.");
    }
    return bRes;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
