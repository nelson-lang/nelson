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
#include "NelsonGateway.hpp"
#include "headcommentsBuiltin.hpp"
#include "htmltopdfBuiltin.hpp"
#include "markdownBuiltin.hpp"
#include "xmldocbuildBuiltin.hpp"
#include "xmldoccheckerBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"help_tools";
//=============================================================================
static const nlsGateway gateway[] = {
    { "markdown", Nelson::HelpToolsGateway::markdownBuiltin, 1, 2 },
    { "headcomments", Nelson::HelpToolsGateway::headcommentsBuiltin, 1, 1 },
    { "xmldocchecker", Nelson::HelpToolsGateway::xmldoccheckerBuiltin, 2, 1 },
    { "xmldocbuild", Nelson::HelpToolsGateway::xmldocbuildBuiltin, 1, 5 },
    { "htmltopdf", Nelson::HelpToolsGateway::htmltopdfBuiltin, 0, 2 },
};
//=============================================================================
static bool
initializeHelpToolsModule(Nelson::Evaluator* eval)
{
    return true;
}
//=============================================================================
static bool
finishHelpToolsModule(Nelson::Evaluator* eval)
{
    return true;
}
//=============================================================================
NLSGATEWAYFUNCEXTENDED(gateway, (void*)initializeHelpToolsModule)
//=============================================================================
NLSGATEWAYINFO(gateway)
//=============================================================================
NLSGATEWAYREMOVEEXTENDED(gateway, (void*)finishHelpToolsModule)
//=============================================================================
NLSGATEWAYNAME()
//=============================================================================
