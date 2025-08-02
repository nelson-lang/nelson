//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "HelpBrowser.hpp"
#include "NelsonGateway.hpp"
#include "helpbrowserBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"help_browser";
//=============================================================================
static const nlsGateway gateway[] = {
    { "helpbrowser", (ptrBuiltin)Nelson::HelpBrowserGateway::helpbrowserBuiltin, 1, 2 },
};
//=============================================================================
static bool
initializeHelpBrowserModule(Nelson::Evaluator* eval)
{
    HelpBrowser::getInstance();
    return true;
}
//=============================================================================
static bool
finishHelpBrowserModule(Nelson::Evaluator* eval)
{
    HelpBrowser::getInstance()->destroy();
    return true;
}
//=============================================================================
NLSGATEWAYFUNCEXTENDED(gateway, (void*)initializeHelpBrowserModule)
//=============================================================================
NLSGATEWAYINFO(gateway)
//=============================================================================
NLSGATEWAYREMOVEEXTENDED(gateway, (void*)finishHelpBrowserModule)
//=============================================================================
NLSGATEWAYNAME()
//=============================================================================
