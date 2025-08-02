//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
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
#include "addgatewayBuiltin.hpp"
#include "removegatewayBuiltin.hpp"
#include "gatewayinfoBuiltin.hpp"
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
    { "modulepath", (ptrBuiltin)Nelson::ModulesManagerGateway::modulepathBuiltin, 1, 2 },
    { "getmodules", (ptrBuiltin)Nelson::ModulesManagerGateway::getmodulesBuiltin, 1, 1 },
    { "ismodule", (ptrBuiltin)Nelson::ModulesManagerGateway::ismoduleBuiltin, 1, 1 },
    { "toolboxdir", (ptrBuiltin)Nelson::ModulesManagerGateway::toolboxdirBuiltin, 1, 1 },
    { "usermodulesdir", (ptrBuiltin)Nelson::ModulesManagerGateway::usermodulesdirBuiltin, 1, 0 },
    { "requiremodule", (ptrBuiltin)Nelson::ModulesManagerGateway::requiremoduleBuiltin, 1, 1 },
    { "semver", (ptrBuiltin)Nelson::ModulesManagerGateway::semverBuiltin, 1, 2 },
    { "addgateway", (ptrBuiltin)Nelson::ModulesManagerGateway::addgatewayBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "removegateway", (ptrBuiltin)Nelson::ModulesManagerGateway::removegatewayBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "gatewayinfo", (ptrBuiltin)Nelson::ModulesManagerGateway::gatewayinfoBuiltin, 2, 1 },
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
