//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NelsonGateway.hpp"
#include "gettextBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"i18n";
//=============================================================================
static const nlsGateway gateway[] = {
    { "gettext", (ptrBuiltin)Nelson::I18nGateway::gettextBuiltin, 1, 1 },
    { "_", (ptrBuiltin)Nelson::I18nGateway::gettextBuiltin, 1, 1 },

};
//=============================================================================
static bool
initializeI18nModule(Nelson::Evaluator* eval)
{
    return true;
}
//=============================================================================
static bool
finishI18nModule(Nelson::Evaluator* eval)
{
    return true;
}
//=============================================================================
NLSGATEWAYFUNCEXTENDED(gateway, (void*)initializeI18nModule)
//=============================================================================
NLSGATEWAYINFO(gateway)
//=============================================================================
NLSGATEWAYREMOVEEXTENDED(gateway, (void*)finishI18nModule)
//=============================================================================
NLSGATEWAYNAME()
//=============================================================================
