//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
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
#include "usernameBuiltin.hpp"
#include "hostnameBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"os_functions";
//=============================================================================
static const nlsGateway gateway[] = {
    { "system", (ptrBuiltin)Nelson::OsFunctionsGateway::systemBuiltin, 2, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "dos", (ptrBuiltin)Nelson::OsFunctionsGateway::systemBuiltin, 2, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "unix", (ptrBuiltin)Nelson::OsFunctionsGateway::systemBuiltin, 2, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "getenv", (ptrBuiltin)Nelson::OsFunctionsGateway::getenvBuiltin, 1, 1 },
    { "setenv", (ptrBuiltin)Nelson::OsFunctionsGateway::setenvBuiltin, 1, 2 },
    { "searchenv", (ptrBuiltin)Nelson::OsFunctionsGateway::searchenvBuiltin, 1, 2 },
    { "ispc", (ptrBuiltin)Nelson::OsFunctionsGateway::ispcBuiltin, 1, 0 },
    { "isunix", (ptrBuiltin)Nelson::OsFunctionsGateway::isunixBuiltin, 1, 0 },
    { "ismac", (ptrBuiltin)Nelson::OsFunctionsGateway::ismacBuiltin, 1, 0 },
    { "computer", (ptrBuiltin)Nelson::OsFunctionsGateway::computerBuiltin, 1, 0 },
    { "createGUID", (ptrBuiltin)Nelson::OsFunctionsGateway::createGUIDBuiltin, 1, 1 },
    { "winopen", (ptrBuiltin)Nelson::OsFunctionsGateway::winopenBuiltin, 0, 1 },
    { "winqueryreg", (ptrBuiltin)Nelson::OsFunctionsGateway::winqueryregBuiltin, 1, -2 },
    { "hostname", (ptrBuiltin)Nelson::OsFunctionsGateway::hostnameBuiltin, 1, 0 },
    { "username", (ptrBuiltin)Nelson::OsFunctionsGateway::usernameBuiltin, 1, 0 },

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
