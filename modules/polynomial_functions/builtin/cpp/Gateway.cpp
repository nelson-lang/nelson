//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NelsonGateway.hpp"
#include "polyval_hornerBuiltin.hpp"
#include "rootsBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"polynomial_functions";
//=============================================================================
static const nlsGateway gateway[] = {
    { "roots", (ptrBuiltin)Nelson::PolynomialFunctionsGateway::rootsBuiltin, 1, 1, CPP_BUILTIN },
    { "polyval_horner", (ptrBuiltin)Nelson::PolynomialFunctionsGateway::polyval_hornerBuiltin, 1, 2,
        CPP_BUILTIN },
};
//=============================================================================
NLS_GATEWAY_FUNC(gateway)
//=============================================================================
NLS_GATEWAY_REMOVE(gateway)
//=============================================================================
