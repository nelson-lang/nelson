//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "AddGateway.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "GatewaysManager.hpp"
#include "ModulesManager.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
AddGateway(
    Evaluator* eval, const std::wstring& dynlibname, const std::wstring& moduleNameAssociated)
{
    std::wstring errorMessage;
    if (!moduleNameAssociated.empty()
        && !ModulesManager::Instance().isModule(moduleNameAssociated)) {
        Error(_W("Invalid module name:") + moduleNameAssociated);
    }
    GatewaysManager::getInstance()->addGateway(eval, dynlibname, errorMessage);
    if (!errorMessage.empty()) {
        Error(errorMessage);
    }
    if (!moduleNameAssociated.empty()) {
        if (!ModulesManager::Instance().setLibraryPath(moduleNameAssociated, dynlibname)) {
            Error(_W("Impossible to associate module name and library path"));
        }
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
