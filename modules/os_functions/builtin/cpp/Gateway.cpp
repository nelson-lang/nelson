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
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"os_functions";
//=============================================================================
static const nlsGateway gateway[] = {
    { "system", (void*)Nelson::OsFunctionsGateway::systemBuiltin, 2, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "dos", (void*)Nelson::OsFunctionsGateway::systemBuiltin, 2, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "unix", (void*)Nelson::OsFunctionsGateway::systemBuiltin, 2, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "getenv", (void*)Nelson::OsFunctionsGateway::getenvBuiltin, 1, 1, CPP_BUILTIN },
    { "setenv", (void*)Nelson::OsFunctionsGateway::setenvBuiltin, 1, 2, CPP_BUILTIN },
    { "searchenv", (void*)Nelson::OsFunctionsGateway::searchenvBuiltin, 1, 2, CPP_BUILTIN },
    { "ispc", (void*)Nelson::OsFunctionsGateway::ispcBuiltin, 1, 0, CPP_BUILTIN },
    { "isunix", (void*)Nelson::OsFunctionsGateway::isunixBuiltin, 1, 0, CPP_BUILTIN },
    { "ismac", (void*)Nelson::OsFunctionsGateway::ismacBuiltin, 1, 0, CPP_BUILTIN },
    { "computer", (void*)Nelson::OsFunctionsGateway::computerBuiltin, 1, 0, CPP_BUILTIN },
    { "createGUID", (void*)Nelson::OsFunctionsGateway::createGUIDBuiltin, 1, 1, CPP_BUILTIN },
    { "winopen", (void*)Nelson::OsFunctionsGateway::winopenBuiltin, 0, 1, CPP_BUILTIN },
    { "winqueryreg", (void*)Nelson::OsFunctionsGateway::winqueryregBuiltin, 1, -2, CPP_BUILTIN },

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
