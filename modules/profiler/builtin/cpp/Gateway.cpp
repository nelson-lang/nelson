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
#include "profileBuiltin.hpp"
#include "profsaveBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"profiler";
//=============================================================================
static const nlsGateway gateway[] = {
    { "profile", (ptrBuiltin)Nelson::ProfilerGateway::profileBuiltin, 1, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "profsave", (ptrBuiltin)Nelson::ProfilerGateway::profsaveBuiltin, 0, 2 },
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
