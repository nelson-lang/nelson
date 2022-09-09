//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "cumprodBuiltin.hpp"
#include "cumulativeFunctionBuiltinHelpers.hpp"
#include "CumProd.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DataAnalysisGateway::cumprodBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    // B = cumprod(A)
    // B = cumprod(A, dim)
    // B = cumprod(A, [dim], nanflag)
    return cumulativeFunctionBuiltin(eval, nLhs, argIn, "cumprod", CumProd);
}
//=============================================================================
