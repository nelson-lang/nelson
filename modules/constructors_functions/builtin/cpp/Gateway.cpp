//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#pragma warning(disable : 4190)
#endif
//=============================================================================
#include "NelsonGateway.hpp"
#include "epsBuiltin.hpp"
#include "eyeBuiltin.hpp"
#include "iBuiltin.hpp"
#include "infBuiltin.hpp"
#include "nanBuiltin.hpp"
#include "onesBuiltin.hpp"
#include "piBuiltin.hpp"
#include "zerosBuiltin.hpp"
#include "diagBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"constructors_functions";
//=============================================================================
static const nlsGateway gateway[] = { { "eye", (ptrBuiltin)Nelson::ConstructorsGateway::eyeBuiltin,
                                          1, 0, CPP_BUILTIN_WITH_EVALUATOR },
    { "i", (ptrBuiltin)Nelson::ConstructorsGateway::iBuiltin, 1, 0 },
    { "j", (ptrBuiltin)Nelson::ConstructorsGateway::iBuiltin, 1, 0 },
    { "nan", (ptrBuiltin)Nelson::ConstructorsGateway::nanBuiltin, 1, 0,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "NaN", (ptrBuiltin)Nelson::ConstructorsGateway::nanBuiltin, 1, 0,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "inf", (ptrBuiltin)Nelson::ConstructorsGateway::infBuiltin, 1, 0,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "Inf", (ptrBuiltin)Nelson::ConstructorsGateway::infBuiltin, 1, 0,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "eps", (ptrBuiltin)Nelson::ConstructorsGateway::epsBuiltin, 1, 0 },
    { "pi", (ptrBuiltin)Nelson::ConstructorsGateway::piBuiltin, 1, 0 },
    { "ones", (ptrBuiltin)Nelson::ConstructorsGateway::onesBuiltin, -1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "zeros", (ptrBuiltin)Nelson::ConstructorsGateway::zerosBuiltin, -1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "diag", (ptrBuiltin)Nelson::ConstructorsGateway::diagBuiltin, 1, 2 } };
//=============================================================================
NLS_GATEWAY_FUNC(gateway)
//=============================================================================
NLS_GATEWAY_REMOVE(gateway)
//=============================================================================
