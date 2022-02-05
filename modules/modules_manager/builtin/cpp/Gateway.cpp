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
#include "GetExternalModulesPath.hpp"
#include "NelsonGateway.hpp"
#include "addmoduleBuiltin.hpp"
#include "getmodulesBuiltin.hpp"
#include "ismoduleBuiltin.hpp"
#include "modulepathBuiltin.hpp"
#include "removemoduleBuiltin.hpp"
#include "requiremoduleBuiltin.hpp"
#include "toolboxdirBuiltin.hpp"
#include "usermodulesdirBuiltin.hpp"
#include "semverBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"modules_manager";
//=============================================================================
static const nlsGateway gateway[] = {
    { "removemodule", (ptrBuiltin)Nelson::ModulesManagerGateway::removemoduleBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "addmodule", (ptrBuiltin)Nelson::ModulesManagerGateway::addmoduleBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "modulepath", (ptrBuiltin)Nelson::ModulesManagerGateway::modulepathBuiltin, 1, 3 },
    { "getmodules", (ptrBuiltin)Nelson::ModulesManagerGateway::getmodulesBuiltin, 1, 1 },
    { "ismodule", (ptrBuiltin)Nelson::ModulesManagerGateway::ismoduleBuiltin, 1, 1 },
    { "toolboxdir", (ptrBuiltin)Nelson::ModulesManagerGateway::toolboxdirBuiltin, 1, 1 },
    { "usermodulesdir", (ptrBuiltin)Nelson::ModulesManagerGateway::usermodulesdirBuiltin, 1, 0 },
    { "requiremodule", (ptrBuiltin)Nelson::ModulesManagerGateway::requiremoduleBuiltin, 1, 1 },
    { "semver", (ptrBuiltin)Nelson::ModulesManagerGateway::semverBuiltin, 1, 2 }
};
//=============================================================================
NLSGATEWAYNAME()
//=============================================================================
static bool
initializeModulesManagerModule(Nelson::Evaluator* eval)
{
    return CreateIfRequiredExternalModulesPath();
}
//=============================================================================
static bool
finishModulesManagerModule(Nelson::Evaluator* eval)
{
    return true;
}
//=============================================================================
NLSGATEWAYFUNCEXTENDED(gateway, (void*)initializeModulesManagerModule)
//=============================================================================
NLSGATEWAYINFO(gateway)
//=============================================================================
NLSGATEWAYREMOVEEXTENDED(gateway, (void*)finishModulesManagerModule)
//=============================================================================
