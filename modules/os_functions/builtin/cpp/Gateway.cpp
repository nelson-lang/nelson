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
#include "computerBuiltin.hpp"
#include "createGUIDBuiltin.hpp"
#include "getenvBuiltin.hpp"
#include "ismacBuiltin.hpp"
#include "ispcBuiltin.hpp"
#include "isunixBuiltin.hpp"
#include "searchenvBuiltin.hpp"
#include "setenvBuiltin.hpp"
#include "systemBuiltin.hpp"
#include "winopenBuiltin.hpp"
#include "winqueryregBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"os_functions";
//=============================================================================
static const nlsGateway gateway[] = {
    { "system", Nelson::OsFunctionsGateway::systemBuiltin, 2, 1 },
    { "dos", Nelson::OsFunctionsGateway::systemBuiltin, 2, 1 },
    { "unix", Nelson::OsFunctionsGateway::systemBuiltin, 2, 1 },
    { "getenv", Nelson::OsFunctionsGateway::getenvBuiltin, 1, 1 },
    { "setenv", Nelson::OsFunctionsGateway::setenvBuiltin, 1, 2 },
    { "searchenv", Nelson::OsFunctionsGateway::searchenvBuiltin, 1, 2 },
    { "ispc", Nelson::OsFunctionsGateway::ispcBuiltin, 1, 0 },
    { "isunix", Nelson::OsFunctionsGateway::isunixBuiltin, 1, 0 },
    { "ismac", Nelson::OsFunctionsGateway::ismacBuiltin, 1, 0 },
    { "computer", Nelson::OsFunctionsGateway::computerBuiltin, 1, 0 },
    { "createGUID", Nelson::OsFunctionsGateway::createGUIDBuiltin, 1, 1 },
    { "winopen", Nelson::OsFunctionsGateway::winopenBuiltin, 0, 1 },
    { "winqueryreg", Nelson::OsFunctionsGateway::winqueryregBuiltin, 1, -2 },

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
