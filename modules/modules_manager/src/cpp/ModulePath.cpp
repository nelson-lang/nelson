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
#include "ModulePath.hpp"
#include "Error.hpp"
#include "FindDynamicLibraryName.hpp"
#include "ModulesHelpers.hpp"
#include "ModulesManager.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
ModulePath(const std::wstring& moduleshortname)
{
    std::wstring rpath;
    return rpath;
}
//=============================================================================
std::wstring
ModulePath(const std::wstring& modulerootpath, const std::wstring& moduleshortname,
    MODULEPATH_OPTION option)
{
    std::filesystem::path p;
    switch (option) {
    case GET_BINARY_PATH: {
        p = createFileSystemPath(ConstructBinariesPath(modulerootpath));
        p = p.generic_string();
        if (!std::filesystem::is_directory(p)) {
            Error(_W("Path does not exist:") + L"\n" + convertFileSytemPathToGenericWString(p));
        }
    } break;
    case GET_ROOT_PATH: {
        p = createFileSystemPath(ConstructRootName(modulerootpath, moduleshortname));
        p = p.generic_string();
        if (!std::filesystem::is_directory(p)) {
            Error(_W("Path does not exist:") + L"\n" + convertFileSytemPathToGenericWString(p));
        }
    } break;
    case GET_ETC_PATH: {
        p = createFileSystemPath(ConstructEtcName(modulerootpath, moduleshortname));
        p = p.generic_string();
        if (!std::filesystem::is_directory(p)) {
            Error(_W("Path does not exist:") + L"\n" + convertFileSytemPathToGenericWString(p));
        }
    } break;
    case GET_DYNLIB_FULLPATH: {
        p = createFileSystemPath(ConstructDynamicLibraryFullname(modulerootpath, moduleshortname));
        p = p.generic_string();
        std::wstring filename
            = FindDynamicLibraryName(convertFileSytemPathToGenericWString(p.parent_path()),
                convertFileSytemPathToGenericWString(p.filename()), false);
        if (filename.empty()) {
            Error(_W("File does not exist:") + L"\n" + convertFileSytemPathToGenericWString(p));
        }
    } break;
    case GET_SCRIPT_PATH: {
        p = createFileSystemPath(ConstructScriptName(modulerootpath, moduleshortname));
        p = p.generic_string();
        if (!std::filesystem::is_directory(p)) {
            Error(_W("Path does not exist:") + L"\n" + convertFileSytemPathToGenericWString(p));
        }
    } break;
    default: {
        Error(_W("Wrong option."));
    } break;
    }
    return convertFileSytemPathToGenericWString(p);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
