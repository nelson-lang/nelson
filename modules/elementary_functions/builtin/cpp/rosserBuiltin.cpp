//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "rosserBuiltin.hpp"
#include "Cast.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include <algorithm>
//=============================================================================
using namespace Nelson;
//=============================================================================
static ArrayOf
rosserDoubleMatrix();
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::rosserBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 1);
    nargoutcheck(nLhs, 0, 1);

    ArrayOf result = rosserDoubleMatrix();
    if (!argIn.empty()) {
        result = Cast(eval, result, argIn[0].asUtf8String());
    }
    return ArrayOfVector { result };
}
//=============================================================================
static ArrayOf
rosserDoubleMatrix()
{
    static constexpr double values[] = { 611, 196, -192, 407, -8, -52, -49, 29, 196, 899, 113, -192,
        -71, -43, -8, -44, -192, 113, 899, 196, 61, 49, 8, 52, 407, -192, 196, 611, 8, 44, 59, -23,
        -8, -71, 61, 8, 411, -599, 208, 208, -52, -43, 49, 44, -599, 411, 208, 208, -49, -8, 8, 59,
        208, 208, 99, -911, 29, -44, 52, -23, 208, 208, -911, 99 };

    auto* data = static_cast<double*>(
        ArrayOf::allocateArrayOf(NLS_DOUBLE, 64, Nelson::stringVector(), false));
    std::copy(values, values + 64, data);
    return ArrayOf(NLS_DOUBLE, Dimensions(8, 8), data);
}
//=============================================================================
