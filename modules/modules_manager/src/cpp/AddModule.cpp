//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "AddModule.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "EvaluateScriptFile.hpp"
#include "FindDynamicLibraryName.hpp"
#include "ModulesHelpers.hpp"
#include "ModulesManager.hpp"
#include "NelsonConfiguration.hpp"
#include "characters_encoding.hpp"
#include "FileSystemWrapper.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
AddModule(Evaluator* eval, const std::wstring& modulerootpath, const std::wstring& moduleshortname)
{
    std::wstring _modulerootpath = FileSystemWrapper::Path::normalize(modulerootpath);
    if (StringHelpers::ends_with(_modulerootpath, L"\\")
        || (StringHelpers::ends_with(_modulerootpath, L"/"))) {
        _modulerootpath.pop_back();
    }
    if (FileSystemWrapper::Path::is_directory(_modulerootpath)) {
        FileSystemWrapper::Path pathmainloader(_modulerootpath);
        pathmainloader += L"/etc/startup.m";
        if (FileSystemWrapper::Path::is_regular_file(pathmainloader)) {
            if (!IsExistingModuleName(moduleshortname) && !IsExistingModulePath(_modulerootpath)) {
                RegisterModule(moduleshortname, _modulerootpath,
                    !NelsonConfiguration::getInstance()->isModulesProtected());
                EvaluateScriptFile(eval, pathmainloader.generic_wstring());
            } else {
                if ((IsExistingModuleName(moduleshortname)
                        && IsExistingModulePath(_modulerootpath))) {
                    Error(L"Nelson:addmodule:moduleAlreadyUsed",
                        L"%s: This module is already used: %s", moduleshortname, moduleshortname);
                } else {
                    if (IsExistingModuleName(moduleshortname)) {
                        Error(L"Nelson:addmodule:moduleNameAlreadyUsed",
                            L"An existing module with the same name already used: %s",
                            moduleshortname);
                    }
                    if (IsExistingModulePath(_modulerootpath)) {
                        Error(L"Nelson:addmodule:modulePathAlreadyUsed",
                            L"An existing module with the same path already defined: \n%s",
                            _modulerootpath);
                    }
                }
            }
        } else {
            Error(L"Nelson:addmodule:startupNotFound", L"startup.m does not exist (%s).",
                moduleshortname);
        }
    } else {
        Error(L"Nelson:addmodule:invalidRootPath", L"An existing module root path expected (%s)",
            moduleshortname);
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
