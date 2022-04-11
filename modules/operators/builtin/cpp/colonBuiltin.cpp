//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "colonBuiltin.hpp"
#include "Error.hpp"
#include "OverloadBinaryOperator.hpp"
#include "OverloadTernaryOperator.hpp"
#include "Colon.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::OperatorsGateway::colonBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    ArrayOf res;
    if (argIn.size() == 2) {
        ArrayOf A = argIn[0];
        ArrayOf B = argIn[1];
        res = eval->colonUnitOperator(argIn[0], argIn[1]);
    } else if (argIn.size() == 3) {
        ArrayOf A = argIn[0];
        ArrayOf B = argIn[1];
        ArrayOf C = argIn[2];
        res = eval->colonOperator(argIn[0], argIn[1], argIn[2]);
    } else {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    retval << res;
    return retval;
}
//=============================================================================
