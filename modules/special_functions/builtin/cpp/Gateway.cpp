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
#include "__interp1__Builtin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"special_functions";
//=============================================================================
static const nlsGateway gateway[] = {
    { "gamma", (ptrBuiltin)Nelson::SpecialFunctionsGateway::gammaBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { OVERLOAD_FUNCTION_NAME(NLS_DOUBLE_STR, "gamma"),
        (ptrBuiltin)Nelson::SpecialFunctionsGateway::generic_gammaBuiltin, 1, 1, CPP_BUILTIN },
    { OVERLOAD_FUNCTION_NAME(NLS_SINGLE_STR, "gamma"),
        (ptrBuiltin)Nelson::SpecialFunctionsGateway::generic_gammaBuiltin, 1, 1, CPP_BUILTIN },
    //=============================================================================
    { "betainc", (ptrBuiltin)Nelson::SpecialFunctionsGateway::betaincBuiltin, 1, 3,
        CPP_BUILTIN_WITH_EVALUATOR },
    { OVERLOAD_FUNCTION_NAME(NLS_DOUBLE_STR, "betainc"),
        (ptrBuiltin)Nelson::SpecialFunctionsGateway::generic_betaincBuiltin, 1, 3, CPP_BUILTIN },
    { OVERLOAD_FUNCTION_NAME(NLS_SINGLE_STR, "betainc"),
        (ptrBuiltin)Nelson::SpecialFunctionsGateway::generic_betaincBuiltin, 1, 3, CPP_BUILTIN },
    //=============================================================================
    { "gcd", (ptrBuiltin)Nelson::SpecialFunctionsGateway::gcdBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { OVERLOAD_FUNCTION_NAME(NLS_DOUBLE_STR, "gcd"),
        (ptrBuiltin)Nelson::SpecialFunctionsGateway::double_gcdBuiltin, 1, 2, CPP_BUILTIN },
    { OVERLOAD_FUNCTION_NAME(NLS_SINGLE_STR, "gcd"),
        (ptrBuiltin)Nelson::SpecialFunctionsGateway::single_gcdBuiltin, 1, 2, CPP_BUILTIN },
    { OVERLOAD_FUNCTION_NAME(NLS_LOGICAL_STR, "gcd"),
        (ptrBuiltin)Nelson::SpecialFunctionsGateway::logical_gcdBuiltin, 1, 2, CPP_BUILTIN },
    { OVERLOAD_FUNCTION_NAME(NLS_UINT8_STR, "gcd"),
        (ptrBuiltin)Nelson::SpecialFunctionsGateway::uint8_gcdBuiltin, 1, 2, CPP_BUILTIN },
    { OVERLOAD_FUNCTION_NAME(NLS_UINT16_STR, "gcd"),
        (ptrBuiltin)Nelson::SpecialFunctionsGateway::uint16_gcdBuiltin, 1, 2, CPP_BUILTIN },
    { OVERLOAD_FUNCTION_NAME(NLS_UINT32_STR, "gcd"),
        (ptrBuiltin)Nelson::SpecialFunctionsGateway::uint32_gcdBuiltin, 1, 2, CPP_BUILTIN },
    { OVERLOAD_FUNCTION_NAME(NLS_UINT64_STR, "gcd"),
        (ptrBuiltin)Nelson::SpecialFunctionsGateway::uint64_gcdBuiltin, 1, 2, CPP_BUILTIN },
    { OVERLOAD_FUNCTION_NAME(NLS_INT8_STR, "gcd"),
        (ptrBuiltin)Nelson::SpecialFunctionsGateway::int8_gcdBuiltin, 1, 2, CPP_BUILTIN },
    { OVERLOAD_FUNCTION_NAME(NLS_INT16_STR, "gcd"),
        (ptrBuiltin)Nelson::SpecialFunctionsGateway::int16_gcdBuiltin, 1, 2, CPP_BUILTIN },
    { OVERLOAD_FUNCTION_NAME(NLS_INT32_STR, "gcd"),
        (ptrBuiltin)Nelson::SpecialFunctionsGateway::int32_gcdBuiltin, 1, 2, CPP_BUILTIN },
    { OVERLOAD_FUNCTION_NAME(NLS_INT64_STR, "gcd"),
        (ptrBuiltin)Nelson::SpecialFunctionsGateway::int64_gcdBuiltin, 1, 2, CPP_BUILTIN },
    { OVERLOAD_FUNCTION_NAME(NLS_CHAR_STR, "gcd"),
        (ptrBuiltin)Nelson::SpecialFunctionsGateway::char_gcdBuiltin, 1, 2, CPP_BUILTIN },

    //=============================================================================
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
