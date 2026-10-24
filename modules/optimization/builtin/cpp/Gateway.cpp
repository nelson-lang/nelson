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
#include "OptimizationHighs.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"optimization";
//=============================================================================
static const nlsGateway gateway[] = {
    { "intlinprog", (ptrBuiltin)Nelson::OptimizationGateway::intlinprogBuiltin, 5, 10 },
    { "linprog", (ptrBuiltin)Nelson::OptimizationGateway::linprogBuiltin, 5, 9 },
};
//=============================================================================
NLS_GATEWAY_FUNC(gateway)
//=============================================================================
NLS_GATEWAY_REMOVE(gateway)
//=============================================================================
