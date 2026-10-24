//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "interp2Builtin.hpp"
#include "InterpolationArguments.hpp"
#include "InterpolationGrid.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// Calls not handled natively (refinement forms, other classes, invalid
// arguments with their errors) run the generic implementation.
static const char* GENERIC_IMPLEMENTATION = "__interp2_generic__";
//=============================================================================
static bool
tryNativeInterp2(const ArrayOfVector& argIn, ArrayOf& result);
//=============================================================================
// Vq = interp2(V, Xq, Yq), interp2(X, Y, V, Xq, Yq), interp2(..., method),
// interp2(..., method, extrapval), interp2(V), interp2(V, k)
ArrayOfVector
Nelson::SpecialFunctionsGateway::interp2Builtin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOf result;
    if (nLhs <= 1 && argIn.size() <= 7 && tryNativeInterp2(argIn, result)) {
        ArrayOfVector retval;
        retval << result;
        return retval;
    }
    return callInterpolationFunction(eval, GENERIC_IMPLEMENTATION, argIn, nLhs);
}
//=============================================================================
static bool
tryNativeInterp2(const ArrayOfVector& argIn, ArrayOf& result)
{
    size_t count = argIn.size();
    std::wstring method;
    bool useConstant = false;
    ArrayOf extrapolationValue;
    if (!parseGriddedInterpolationTail(argIn, count, method, useConstant, extrapolationValue)) {
        return false;
    }
    if (count == 3) {
        return Interpolation2D(nullptr, nullptr, argIn[0], argIn[1], argIn[2], method, useConstant,
            extrapolationValue, result);
    }
    if (count == 5) {
        return Interpolation2D(&argIn[0], &argIn[1], argIn[2], argIn[3], argIn[4], method,
            useConstant, extrapolationValue, result);
    }
    return false;
}
//=============================================================================
