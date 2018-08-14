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
#include "addpathBuiltin.hpp"
#include "builtinBuiltin.hpp"
#include "clearfunBuiltin.hpp"
#include "fevalBuiltin.hpp"
#include "isbuiltinBuiltin.hpp"
#include "ismacroBuiltin.hpp"
#include "macroargsBuiltin.hpp"
#include "pathBuiltin.hpp"
#include "rehashBuiltin.hpp"
#include "restoredefaultpathBuiltin.hpp"
#include "rmpathBuiltin.hpp"
#include "userpathBuiltin.hpp"
#include "whatBuiltin.hpp"
#include "whichBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"functions_manager";
//=============================================================================
static const nlsGateway gateway[] = {
    { "which", Nelson::FunctionsGateway::whichBuiltin, 1, 1 },
    { "macroargs", Nelson::FunctionsGateway::macroargsBuiltin, 2, 1 },
    { "builtin", Nelson::FunctionsGateway::builtinBuiltin, -1, -1 },
    { "feval", Nelson::FunctionsGateway::fevalBuiltin, -1, -1 },
    { "clearfun", Nelson::FunctionsGateway::clearfunBuiltin, 1, 1 },
    { "what", Nelson::FunctionsGateway::whatBuiltin, -1, 0 },
    { "addpath", Nelson::FunctionsGateway::addpathBuiltin, 1, -1 },
    { "rmpath", Nelson::FunctionsGateway::rmpathBuiltin, 1, -1 },
    { "path", Nelson::FunctionsGateway::pathBuiltin, 1, 2 },
    { "restoredefaultpath", Nelson::FunctionsGateway::restoredefaultpathBuiltin, 0, 0 },
    { "rehash", Nelson::FunctionsGateway::rehashBuiltin, 0, 0 },
    { "userpath", Nelson::FunctionsGateway::userpathBuiltin, 1, 1 },
    { "ismacro", Nelson::FunctionsGateway::ismacroBuiltin, 1, 1 },
    { "isbuiltin", Nelson::FunctionsGateway::isbuiltinBuiltin, 1, 1 },
};
//=============================================================================
NLSGATEWAYFUNC(gateway)
//=============================================================================
NLSGATEWAYINFO(gateway)
//=============================================================================
NLSGATEWAYREMOVE(gateway)
//=============================================================================
NLSGATEWAYNAME()
//=============================================================================
