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
#include "EvaluateScriptFile.hpp"
#include "ModulesManager.hpp"
#include "NelsonConfiguration.hpp"
#include "FileSystemWrapper.hpp"
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
            Error(moduleshortname + _W(": This module is registered but it has no path."));
        }
        if (FileSystemWrapper::Path::is_directory(rootpathmodule)) {
            FileSystemWrapper::Path pathfinish(rootpathmodule);
            pathfinish += L"/etc/finish.m";
            if (FileSystemWrapper::Path::is_regular_file(pathfinish)) {
                EvaluateScriptFile(eval, pathfinish.generic_wstring());
            } else {
                Error(_W("finish.m does not exist."));
            }
            return UnregisterModule(moduleshortname);
        }
        Error(_W("An existing module root path expected."));
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
