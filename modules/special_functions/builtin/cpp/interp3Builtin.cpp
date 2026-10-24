//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "interp3Builtin.hpp"
#include "InterpolationArguments.hpp"
#include "InterpolationGrid.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// Calls not handled natively (refinement forms, other classes, invalid
// arguments with their errors) run the generic implementation.
static const char* GENERIC_IMPLEMENTATION = "__interp3_generic__";
//=============================================================================
static bool
tryNativeInterp3(const ArrayOfVector& argIn, ArrayOf& result);
//=============================================================================
// Vq = interp3(V, Xq, Yq, Zq), interp3(X, Y, Z, V, Xq, Yq, Zq),
// interp3(..., method), interp3(..., method, extrapval), interp3(V), interp3(V, k)
ArrayOfVector
Nelson::SpecialFunctionsGateway::interp3Builtin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOf result;
    if (nLhs <= 1 && argIn.size() <= 9 && tryNativeInterp3(argIn, result)) {
        ArrayOfVector retval;
        retval << result;
        return retval;
    }
    return callInterpolationFunction(eval, GENERIC_IMPLEMENTATION, argIn, nLhs);
}
//=============================================================================
static bool
tryNativeInterp3(const ArrayOfVector& argIn, ArrayOf& result)
{
    size_t count = argIn.size();
    std::wstring method;
    bool useConstant = false;
    ArrayOf extrapolationValue;
    if (!parseGriddedInterpolationTail(argIn, count, method, useConstant, extrapolationValue)) {
        return false;
    }
    if (count == 4) {
        return Interpolation3D(nullptr, nullptr, nullptr, argIn[0], argIn[1], argIn[2], argIn[3],
            method, useConstant, extrapolationValue, result);
    }
    if (count == 7) {
        return Interpolation3D(&argIn[0], &argIn[1], &argIn[2], argIn[3], argIn[4], argIn[5],
            argIn[6], method, useConstant, extrapolationValue, result);
    }
    return false;
}
//=============================================================================
