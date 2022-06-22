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
#include "backgroundPool_getBuiltin.hpp"
#include "backgroundPool_displayBuiltin.hpp"
#include "backgroundPool_usedBuiltin.hpp"
#include "backgroundPool_deleteBuiltin.hpp"
#include "backgroundPool_fieldnamesBuiltin.hpp"
#include "backgroundPool_structBuiltin.hpp"
#include "parfevalBuiltin.hpp"
#include "FevalFuture_displayBuiltin.hpp"
#include "FevalFuture_getBuiltin.hpp"
#include "FevalFuture_usedBuiltin.hpp"
#include "FevalQueue_displayBuiltin.hpp"
#include "FevalQueue_getBuiltin.hpp"
#include "FevalQueue_usedBuiltin.hpp"
#include "fetchOutputsBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"parallel";
//=============================================================================
static const nlsGateway gateway[] = {
    { "backgroundPool_get", (ptrBuiltin)Nelson::ParallelGateway::backgroundPool_getBuiltin, 1, 2 },
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
    { "backgroundPool_fieldnames",
        (ptrBuiltin)Nelson::ParallelGateway::backgroundPool_fieldnamesBuiltin, 1, 1 },
    { "backgroundPool_struct", (ptrBuiltin)Nelson::ParallelGateway::backgroundPool_structBuiltin, 1,
        1 },
    { "parfeval", (ptrBuiltin)Nelson::ParallelGateway::parfevalBuiltin, 1, -3,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "FevalFuture_display", (ptrBuiltin)Nelson::ParallelGateway::FevalFuture_displayBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "FevalFuture_disp", (ptrBuiltin)Nelson::ParallelGateway::FevalFuture_displayBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "FevalFuture_used", (ptrBuiltin)Nelson::ParallelGateway::FevalFuture_usedBuiltin, 1, 0 },
    { "fetchOutputs", (ptrBuiltin)Nelson::ParallelGateway::fetchOutputsBuiltin, -1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "FevalQueue_display", (ptrBuiltin)Nelson::ParallelGateway::FevalQueue_displayBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "FevalQueue_disp", (ptrBuiltin)Nelson::ParallelGateway::FevalQueue_displayBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "FevalQueue_used", (ptrBuiltin)Nelson::ParallelGateway::FevalQueue_usedBuiltin, 1, 0 },
    { "FevalFuture_get", (ptrBuiltin)Nelson::ParallelGateway::FevalFuture_getBuiltin, 1, 2 },
    { "FevalFuture_used", (ptrBuiltin)Nelson::ParallelGateway::FevalFuture_usedBuiltin, 1, 0 },
    { "FevalQueue_get", (ptrBuiltin)Nelson::ParallelGateway::FevalQueue_getBuiltin, 1, 2 },

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
