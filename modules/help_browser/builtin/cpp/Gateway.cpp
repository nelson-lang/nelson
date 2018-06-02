//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
    { "helpbrowser", Nelson::HelpBrowserGateway::helpbrowserBuiltin, 1, 2 },
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
