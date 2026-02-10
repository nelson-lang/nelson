//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "RemoveModule.hpp"
#include "Error.hpp"
#include "Warning.hpp"
#include "i18n.hpp"
#include "PredefinedErrorMessages.hpp"
#include "EvaluateScriptFile.hpp"
#include "ModulesManager.hpp"
#include "NelsonConfiguration.hpp"
#include "FileSystemWrapper.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
RemoveModule(Evaluator* eval, const std::wstring& moduleshortname)
{
    if (IsExistingModuleName(moduleshortname)) {
        bool isProtected = IsProtectedModuleName(moduleshortname);
        if (isProtected && NelsonConfiguration::getInstance()->isModulesProtected()) {
            Warning(L"Nelson:module:protected",
                _W("Cannot remove module (protected): ") + moduleshortname);
            return false;
        }
        std::wstring rootpathmodule = GetModulePath(moduleshortname);
        if (rootpathmodule.empty()) {
            raiseError(L"Nelson:modules_manager:ERROR_MODULE_REGISTERED_WITHOUT_PATH",
                ERROR_MODULE_REGISTERED_WITHOUT_PATH, moduleshortname);
        }
        if (FileSystemWrapper::Path::is_directory(rootpathmodule)) {
            FileSystemWrapper::Path pathfinish(rootpathmodule);
            pathfinish += L"/etc/finish.m";
            if (FileSystemWrapper::Path::is_regular_file(pathfinish)) {
                EvaluateScriptFile(eval, pathfinish.generic_wstring());
            } else {
                raiseError(L"Nelson:modules_manager:ERROR_FINISH_M_DOES_NOT_EXIST",
                    ERROR_FINISH_M_DOES_NOT_EXIST);
            }
            return UnregisterModule(moduleshortname);
        }
        raiseError(L"Nelson:modules_manager:ERROR_AN_EXISTING_MODULE_ROOT_PATH_EXPECTED",
            ERROR_AN_EXISTING_MODULE_ROOT_PATH_EXPECTED, moduleshortname);
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
