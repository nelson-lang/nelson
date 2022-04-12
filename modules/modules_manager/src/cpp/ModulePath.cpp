//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ModulePath.hpp"
#include "Error.hpp"
#include "FindDynamicLibraryName.hpp"
#include "ModulesHelpers.hpp"
#include "ModulesManager.hpp"
#include <boost/filesystem.hpp>
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
    boost::filesystem::path p;
    switch (option) {
    case GET_BINARY_PATH: {
        p = ConstructBinariesPath(modulerootpath);
        p = p.generic_wstring();
        if (!boost::filesystem::is_directory(p)) {
            Error(_W("Path does not exist:") + L"\n" + p.generic_wstring());
        }
    } break;
    case GET_ROOT_PATH: {
        p = ConstructRootName(modulerootpath, moduleshortname);
        p = p.generic_wstring();
        if (!boost::filesystem::is_directory(p)) {
            Error(_W("Path does not exist:") + L"\n" + p.generic_wstring());
        }
    } break;
    case GET_ETC_PATH: {
        p = ConstructEtcName(modulerootpath, moduleshortname);
        p = p.generic_wstring();
        if (!boost::filesystem::is_directory(p)) {
            Error(_W("Path does not exist:") + L"\n" + p.generic_wstring());
        }
    } break;
    case GET_DYNLIB_FULLPATH: {
        p = ConstructDynamicLibraryFullname(modulerootpath, moduleshortname);
        p = p.generic_wstring();
        std::wstring filename = FindDynamicLibraryName(
            p.parent_path().generic_wstring(), p.filename().generic_wstring(), false);
        if (filename.empty()) {
            Error(_W("File does not exist:") + L"\n" + p.generic_wstring());
        }
    } break;
    case GET_SCRIPT_PATH: {
        p = ConstructScriptName(modulerootpath, moduleshortname);
        p = p.generic_wstring();
        if (!boost::filesystem::is_directory(p)) {
            Error(_W("Path does not exist:") + L"\n" + p.generic_wstring());
        }
    } break;
    default: {
        Error(_W("Wrong option."));
    } break;
    }
    return p.generic_wstring();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
