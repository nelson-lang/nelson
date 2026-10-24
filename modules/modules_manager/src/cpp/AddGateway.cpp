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
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "GatewaysManager.hpp"
#include "ModulesManager.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
AddGateway(Evaluator* eval, const std::wstring& dynlibname,
    const std::wstring& moduleNameAssociated, bool forceLoaded)
{
    std::wstring errorMessage;
    if (!moduleNameAssociated.empty()
        && !ModulesManager::Instance().isModule(moduleNameAssociated)) {
        Error(L"Nelson:addgateway:invalidModuleName", L"Invalid module name:%s",
            moduleNameAssociated);
    }
    GatewaysManager::getInstance()->addGateway(eval, dynlibname, errorMessage, forceLoaded);
    if (!errorMessage.empty()) {
        Error(L"Nelson:addgateway:loadFailed", errorMessage);
    }
    if (!moduleNameAssociated.empty()) {
        if (!ModulesManager::Instance().setLibraryPath(moduleNameAssociated, dynlibname)) {
            Error(L"Nelson:addgateway:cannotAssociate",
                L"Impossible to associate module name and library path");
        }
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
