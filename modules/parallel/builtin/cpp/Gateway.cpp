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
#include "backgroundPoolBuiltin.hpp"
#include "backgroundPool_displayBuiltin.hpp"
#include "backgroundPool_usedBuiltin.hpp"
#include "backgroundPool_deleteBuiltin.hpp"
#include "parfevalBuiltin.hpp"
#include "fevalFuture_displayBuiltin.hpp"
#include "fetchOutputsBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"parallel";
//=============================================================================
static const nlsGateway gateway[] = {
    { "backgroundPool", (ptrBuiltin)Nelson::ParallelGateway::backgroundPoolBuiltin, 1, 0,
        CPP_BUILTIN },
    { "backgroundPool_display", (ptrBuiltin)Nelson::ParallelGateway::backgroundPool_displayBuiltin,
        0, 2, CPP_BUILTIN_WITH_EVALUATOR },
    { "backgroundPool_disp", (ptrBuiltin)Nelson::ParallelGateway::backgroundPool_displayBuiltin, 0,
        1, CPP_BUILTIN_WITH_EVALUATOR },
    { "backgroundPool_delete", (ptrBuiltin)Nelson::ParallelGateway::backgroundPool_deleteBuiltin, 0,
        1 },
    { "backgroundPool_used", (ptrBuiltin)Nelson::ParallelGateway::backgroundPool_usedBuiltin, 1,
        0 },
    { "parfeval", (ptrBuiltin)Nelson::ParallelGateway::parfevalBuiltin, 1, -3,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "fevalFuture_display", (ptrBuiltin)Nelson::ParallelGateway::fevalFuture_displayBuiltin,
        0, 2, CPP_BUILTIN_WITH_EVALUATOR },
    { "fevalFuture_disp", (ptrBuiltin)Nelson::ParallelGateway::fevalFuture_displayBuiltin, 0,
        1, CPP_BUILTIN_WITH_EVALUATOR },
    { "fetchOutputs", (ptrBuiltin)Nelson::ParallelGateway::fetchOutputsBuiltin,
        -1, 1, CPP_BUILTIN },

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
