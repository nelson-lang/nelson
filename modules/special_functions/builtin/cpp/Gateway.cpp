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
#include "gammalnBuiltin.hpp"
#include "betaincBuiltin.hpp"
#include "gcdBuiltin.hpp"
#include "__interp1__Builtin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"special_functions";
//=============================================================================
static const nlsGateway gateway[] = {
    { "gcd", (ptrBuiltin)Nelson::SpecialFunctionsGateway::gcdBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { "gamma", (ptrBuiltin)Nelson::SpecialFunctionsGateway::gammaBuiltin, 1, 1 },
    { "gammaln", (ptrBuiltin)Nelson::SpecialFunctionsGateway::gammalnBuiltin, 1, 1 },
    { "betainc", (ptrBuiltin)Nelson::SpecialFunctionsGateway::betaincBuiltin, 1, 3 },
    { "__interp1__", (ptrBuiltin)Nelson::SpecialFunctionsGateway::__interp1__Builtin, 1, 3,
        CPP_BUILTIN },
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
