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
#include "i18n.hpp"
#include "TranslationManager.hpp"
#include "PredefinedErrorMessages.hpp"
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
                TranslationManager::getInstance().loadModuleTranslation(moduleshortname,
                    _modulerootpath, NelsonConfiguration::getInstance()->getCurrentLocale());
                RegisterModule(moduleshortname, _modulerootpath,
                    !NelsonConfiguration::getInstance()->isModulesProtected());
                EvaluateScriptFile(eval, pathmainloader.generic_wstring());
            } else {
                if ((IsExistingModuleName(moduleshortname)
                        && IsExistingModulePath(_modulerootpath))) {
                    raiseError(L"Nelson:modules_manager:ModuleAlreadyUsed",
                        ERROR_MODULE_ALREADY_USED, moduleshortname);

                } else if (IsExistingModuleName(moduleshortname)) {
                    raiseError(L"Nelson:modules_manager:ERROR_AN_EXISTING_MODULE_WITH_THE_SAME_"
                               L"NAME_ALREADY_USED",
                        ERROR_AN_EXISTING_MODULE_WITH_THE_SAME_NAME_ALREADY_USED, moduleshortname);
                } else {
                    raiseError(L"Nelson:modules_manager:ERROR_AN_EXISTING_MODULE_WITH_THE_SAME_"
                               L"PATH_ALREADY_DEFINED",
                        ERROR_AN_EXISTING_MODULE_WITH_THE_SAME_PATH_ALREADY_DEFINED,
                        _modulerootpath);
                }
            }
        } else {
            raiseError(L"Nelson:modules_manager:ERROR_STARTUP_M_DOES_NOT_EXIST",
                ERROR_STARTUP_M_DOES_NOT_EXIST, moduleshortname);
        }
    } else {
        raiseError(L"Nelson:modules_manager:ERROR_AN_EXISTING_MODULE_ROOT_PATH_EXPECTED",
            ERROR_AN_EXISTING_MODULE_ROOT_PATH_EXPECTED, moduleshortname);
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
