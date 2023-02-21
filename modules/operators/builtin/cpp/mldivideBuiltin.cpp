//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "mldivideBuiltin.hpp"
#include "Error.hpp"
#include "OverloadBinaryOperator.hpp"
#include "CheckerHelpers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::OperatorsGateway::mldivideBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval(1);
    nargincheck(argIn, 2, 2);
    retval << eval->leftDivideOperator(argIn[0], argIn[1]);
    return retval;
}
//=============================================================================
