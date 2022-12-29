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
#include "gammaBuiltin.hpp"
#include "betaincBuiltin.hpp"
#include "gcdBuiltin.hpp"
#include "interp1Builtin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"special_functions";
//=============================================================================
static const nlsGateway gateway[] = {
    { "gamma", (ptrBuiltin)Nelson::SpecialFunctionsGateway::gammaBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "betainc", (ptrBuiltin)Nelson::SpecialFunctionsGateway::betaincBuiltin, 1, 3,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "gcd", (ptrBuiltin)Nelson::SpecialFunctionsGateway::gcdBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "interp1", (ptrBuiltin)Nelson::SpecialFunctionsGateway::interp1Builtin, 1, 3,
        CPP_BUILTIN_WITH_EVALUATOR },
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
