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
#include "gammaBuiltin.hpp"
#include "gammalnBuiltin.hpp"
#include "erfBuiltin.hpp"
#include "erfcBuiltin.hpp"
#include "erfinvBuiltin.hpp"
#include "erfcinvBuiltin.hpp"
#include "erfcxBuiltin.hpp"
#include "betaincBuiltin.hpp"
#include "gammaincBuiltin.hpp"
#include "gcdBuiltin.hpp"
#include "isprimeBuiltin.hpp"
#include "splineBuiltin.hpp"
#include "__interp1__Builtin.hpp"
#include "__interpn__Builtin.hpp"
#include "interp1Builtin.hpp"
#include "interp2Builtin.hpp"
#include "interp3Builtin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"special_functions";
//=============================================================================
static const nlsGateway gateway[] = {
    { "gcd", (ptrBuiltin)Nelson::SpecialFunctionsGateway::gcdBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { "isprime", (ptrBuiltin)Nelson::SpecialFunctionsGateway::isprimeBuiltin, 1, 1 },
    { "spline", (ptrBuiltin)Nelson::SpecialFunctionsGateway::splineBuiltin, 1, 3, CPP_BUILTIN },
    { "gamma", (ptrBuiltin)Nelson::SpecialFunctionsGateway::gammaBuiltin, 1, 1 },
    { "gammaln", (ptrBuiltin)Nelson::SpecialFunctionsGateway::gammalnBuiltin, 1, 1 },
    { "erf", (ptrBuiltin)Nelson::SpecialFunctionsGateway::erfBuiltin, 1, 1 },
    { "erfc", (ptrBuiltin)Nelson::SpecialFunctionsGateway::erfcBuiltin, 1, 1 },
    { "erfinv", (ptrBuiltin)Nelson::SpecialFunctionsGateway::erfinvBuiltin, 1, 1 },
    { "erfcinv", (ptrBuiltin)Nelson::SpecialFunctionsGateway::erfcinvBuiltin, 1, 1 },
    { "erfcx", (ptrBuiltin)Nelson::SpecialFunctionsGateway::erfcxBuiltin, 1, 1 },
    { "betainc", (ptrBuiltin)Nelson::SpecialFunctionsGateway::betaincBuiltin, 1, 3 },
    { "gammainc", (ptrBuiltin)Nelson::SpecialFunctionsGateway::gammaincBuiltin, 1, 3 },
    { "__interp1__", (ptrBuiltin)Nelson::SpecialFunctionsGateway::__interp1__Builtin, 1, 5,
        CPP_BUILTIN },
    { "__interpn__", (ptrBuiltin)Nelson::SpecialFunctionsGateway::__interpn__Builtin, 1, -1,
        CPP_BUILTIN },
    { "interp1", (ptrBuiltin)Nelson::SpecialFunctionsGateway::interp1Builtin, -1, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "interp2", (ptrBuiltin)Nelson::SpecialFunctionsGateway::interp2Builtin, 1, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "interp3", (ptrBuiltin)Nelson::SpecialFunctionsGateway::interp3Builtin, 1, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
};
//=============================================================================
NLS_GATEWAY_FUNC(gateway)
//=============================================================================
NLS_GATEWAY_REMOVE(gateway)
//=============================================================================
