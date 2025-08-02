//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "cumsumBuiltin.hpp"
#include "cumulativeFunctionBuiltinHelpers.hpp"
#include "CumSum.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DataAnalysisGateway::cumsumBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    // B = cumsum(A)
    // B = cumsum(A, dim)
    // B = cumsum(A, [dim], nanflag)
    return cumulativeFunctionBuiltin(nLhs, argIn, "cumsum", CumSum);
}
//=============================================================================
