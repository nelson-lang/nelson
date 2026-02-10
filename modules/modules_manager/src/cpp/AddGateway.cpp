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
#include "PredefinedErrorMessages.hpp"
#include "GatewaysManager.hpp"
#include "ModulesManager.hpp"
#include "characters_encoding.hpp"
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
        raiseError(L"Nelson:modules_manager:ERROR_INVALID_MODULE_NAME", ERROR_INVALID_MODULE_NAME,
            moduleNameAssociated);
    }
    GatewaysManager::getInstance()->addGateway(eval, dynlibname, errorMessage);
    if (!errorMessage.empty()) {
        Error(errorMessage, L"Nelson:modules_manager:addgateway");
    }
    if (!moduleNameAssociated.empty()) {
        if (!ModulesManager::Instance().setLibraryPath(moduleNameAssociated, dynlibname)) {
            raiseError(L"Nelson:modules_manager:ERROR_IMPOSSIBLE_TO_ASSOCIATE_MODULE_NAME_AND_"
                       L"LIBRARY_PATH",
                ERROR_IMPOSSIBLE_TO_ASSOCIATE_MODULE_NAME_AND_LIBRARY_PATH);
        }
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
