//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "geBuiltin.hpp"
#include "Error.hpp"
#include "GreaterEquals.hpp"
#include "CheckerHelpers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::OperatorsGateway::geBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 2);
    ArrayOfVector retval(nLhs);
    ArrayOf arg1 = argIn[0];
    ArrayOf arg2 = argIn[1];
    retval << eval->geOperator(arg1, arg2);
    return retval;
}
//=============================================================================
