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
#include "OverloadName.hpp"
#include "saveasBuiltin.hpp"
#include "copygraphicsBuiltin.hpp"
#include "imreadBuiltin.hpp"
#include "imwriteBuiltin.hpp"
#include "imformatsBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"graphics_io";
//=============================================================================
static const nlsGateway gateway[] = {
    { "saveas", (ptrBuiltin)Nelson::GraphicsIoGateway::saveasBuiltin, 0, 2, CPP_BUILTIN },
    { "copygraphics", (ptrBuiltin)Nelson::GraphicsIoGateway::copygraphicsBuiltin, 0, 1,
        CPP_BUILTIN },
    { "imread", (ptrBuiltin)Nelson::GraphicsIoGateway::imreadBuiltin, -1, 1, CPP_BUILTIN },
    { "imwrite", (ptrBuiltin)Nelson::GraphicsIoGateway::imwriteBuiltin, 0, -2, CPP_BUILTIN },
    { "imformats", (ptrBuiltin)Nelson::GraphicsIoGateway::imformatsBuiltin, -1, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
};
//=============================================================================
static bool
initializeGraphicsIoModule(Nelson::Evaluator* eval)
{
    return true;
}
//=============================================================================
static bool
finishGraphicsIoModule(Nelson::Evaluator* eval)
{
    return true;
}
//=============================================================================
NLSGATEWAYFUNCEXTENDED(gateway, (void*)initializeGraphicsIoModule)
//=============================================================================
NLSGATEWAYINFO(gateway)
//=============================================================================
NLSGATEWAYREMOVEEXTENDED(gateway, (void*)finishGraphicsIoModule)
//=============================================================================
NLSGATEWAYNAME()
//=============================================================================
