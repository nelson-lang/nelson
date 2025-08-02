//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NelsonGateway.hpp"
#include "Rng.hpp"
#include "randBuiltin.hpp"
#include "randnBuiltin.hpp"
#include "rngBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"random";
//=============================================================================
static const nlsGateway gateway[] = {
    { "rand", (ptrBuiltin)Nelson::RandomGateway::randBuiltin, 1, -1, CPP_BUILTIN },
    { "randn", (ptrBuiltin)Nelson::RandomGateway::randnBuiltin, 1, -1, CPP_BUILTIN },
    { "rng", (ptrBuiltin)Nelson::RandomGateway::rngBuiltin, 2, 1, CPP_BUILTIN },
};
//=============================================================================
static bool
initializeRandModule(Nelson::Evaluator* eval)
{
    RngSetDefault();
    return true;
}
//=============================================================================
static bool
finishRandModule(Nelson::Evaluator* eval)
{
    RngDelete();
    return true;
}
//=============================================================================
NLSGATEWAYFUNCEXTENDED(gateway, (void*)initializeRandModule)
//=============================================================================
NLSGATEWAYINFO(gateway)
//=============================================================================
NLSGATEWAYREMOVEEXTENDED(gateway, (void*)finishRandModule)
//=============================================================================
NLSGATEWAYNAME()
//=============================================================================
