//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NelsonGateway.hpp"
#include "pyrunBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"python_engine";
//=============================================================================
static const nlsGateway gateway[] = {
    { "pyrun", (ptrBuiltin)Nelson::Python_engineGateway::pyrunBuiltin, -1, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
};
//=============================================================================
static bool
initializePythonEngineModule(Nelson::Evaluator* eval)
{
    return true;
}
//=============================================================================
static bool
finishPythonEngineModule(Nelson::Evaluator* eval)
{
    return true;
}
//=============================================================================
NLSGATEWAYFUNCEXTENDED(gateway, (void*)initializePythonEngineModule)
//=============================================================================
NLSGATEWAYINFO(gateway)
//=============================================================================
NLSGATEWAYREMOVEEXTENDED(gateway, (void*)finishPythonEngineModule)
//=============================================================================
NLSGATEWAYNAME()
//=============================================================================
