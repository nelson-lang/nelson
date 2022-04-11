//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <boost/algorithm/string/predicate.hpp>
#include <boost/filesystem.hpp>
#include "AddModule.hpp"
#include "Error.hpp"
#include "EvaluateScriptFile.hpp"
#include "FindDynamicLibraryName.hpp"
#include "ModulesHelpers.hpp"
#include "ModulesManager.hpp"
#include "NelsonConfiguration.hpp"
#include "characters_encoding.hpp"
#include "NormalizePath.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
AddModule(Evaluator* eval, const std::wstring& modulerootpath, const std::wstring& moduleshortname)
{
    std::wstring _modulerootpath = NormalizePath(modulerootpath);
    if (boost::algorithm::ends_with(_modulerootpath, L"\\")
        || (boost::algorithm::ends_with(_modulerootpath, L"/"))) {
        _modulerootpath.pop_back();
    }
    if (boost::filesystem::is_directory(_modulerootpath)) {
        boost::filesystem::path pathmainloader(_modulerootpath);
        pathmainloader += L"/etc/startup.m";
        if (boost::filesystem::exists(pathmainloader)
            && !boost::filesystem::is_directory(pathmainloader)) {
            if (!IsExistingModuleName(moduleshortname) && !IsExistingModulePath(_modulerootpath)) {
                RegisterModule(moduleshortname, _modulerootpath,
                    !NelsonConfiguration::getInstance()->isModulesProtected());
                EvaluateScriptFile(eval, pathmainloader.generic_wstring());
            } else {
                if ((IsExistingModuleName(moduleshortname)
                        && IsExistingModulePath(_modulerootpath))) {
                    Error(
                        moduleshortname + _W(": This module is already used: ") + moduleshortname);
                } else {
                    if (IsExistingModuleName(moduleshortname)) {
                        Error(_W("An existing module with the same name already used: ")
                            + moduleshortname);
                    }
                    if (IsExistingModulePath(_modulerootpath)) {
                        Error(_W("An existing module with the same path already defined: \n")
                            + _modulerootpath);
                    }
                }
            }
        } else {
            Error(_W("startup.m does not exist") + L" (" + moduleshortname + L").");
        }
    } else {
        Error(_W("An existing module root path expected") + L" (" + moduleshortname + L")");
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
