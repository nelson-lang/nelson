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
#include "randiBuiltin.hpp"
#include "randnBuiltin.hpp"
#include "rngBuiltin.hpp"
#include "__rng_eval__Builtin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"random";
//=============================================================================
static const nlsGateway gateway[] = {
    { "rand", (ptrBuiltin)Nelson::RandomGateway::randBuiltin, 1, -1, CPP_BUILTIN_WITH_EVALUATOR },
    { "randi", (ptrBuiltin)Nelson::RandomGateway::randiBuiltin, 1, -1, CPP_BUILTIN },
    { "randn", (ptrBuiltin)Nelson::RandomGateway::randnBuiltin, 1, -1, CPP_BUILTIN_WITH_EVALUATOR },
    { "rng", (ptrBuiltin)Nelson::RandomGateway::rngBuiltin, 2, 1, CPP_BUILTIN },
    { "__rng_eval__", (ptrBuiltin)Nelson::RandomGateway::__rng_eval__Builtin, -1, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
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
NLS_GATEWAY_FUNC_EXTENDED_LAZY(gateway, (void*)initializeRandModule)
//=============================================================================
NLS_GATEWAY_REMOVE_EXTENDED(gateway, (void*)finishRandModule)
//=============================================================================
