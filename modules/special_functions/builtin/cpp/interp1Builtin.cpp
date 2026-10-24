//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cmath>
#include "interp1Builtin.hpp"
#include "Interpolation1D.hpp"
#include "InterpolationArguments.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// Calls not handled natively (pp form, other classes, invalid arguments with
// their errors and warnings) run the generic implementation.
static const char* GENERIC_IMPLEMENTATION = "__interp1_generic__";
//=============================================================================
static bool
tryNativeInterp1(Evaluator* eval, const ArrayOfVector& argIn, ArrayOf& result);
static bool
isKnownMethod(const std::wstring& method);
//=============================================================================
// vq = interp1(x, v, xq), interp1(v, xq), interp1(..., method),
// interp1(..., method, extrapolation), pp = interp1(x, v, method, 'pp')
ArrayOfVector
Nelson::SpecialFunctionsGateway::interp1Builtin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOf result;
    if (nLhs <= 1 && argIn.size() >= 2 && argIn.size() <= 5
        && tryNativeInterp1(eval, argIn, result)) {
        ArrayOfVector retval;
        retval << result;
        return retval;
    }
    return callInterpolationFunction(eval, GENERIC_IMPLEMENTATION, argIn, nLhs);
}
//=============================================================================
static bool
tryNativeInterp1(Evaluator* eval, const ArrayOfVector& argIn, ArrayOf& result)
{
    size_t count = argIn.size();
    std::wstring text;
    if (isInterpolationText(argIn[count - 1])
        && (!readInterpolationText(argIn[count - 1], text) || text == L"pp")) {
        return false;
    }
    std::wstring extrapolationMode = L"default";
    ArrayOf extrapolationValue = ArrayOf::doubleScalar(std::nan(""));
    const ArrayOf& tail = argIn[count - 1];
    if (isInterpolationText(tail)) {
        if (text == L"extrap") {
            extrapolationMode = L"extrap";
            count--;
        }
    } else if (tail.isNumeric() && tail.isScalar() && count >= 4) {
        extrapolationMode = L"constant";
        extrapolationValue = tail;
        count--;
    }
    std::wstring method = L"linear";
    if (count >= 1 && isInterpolationText(argIn[count - 1])) {
        if (!readInterpolationText(argIn[count - 1], method)) {
            return false;
        }
        if (method == L"v5cubic") {
            method = L"cubic";
        }
        if (!isKnownMethod(method)) {
            return false;
        }
        count--;
    }
    Interpolation1DSpline spline = [eval](const ArrayOf& x, const ArrayOf& y, const ArrayOf& q) {
        ArrayOfVector args;
        args << x << y << q;
        return callInterpolationFunction(eval, "spline", args, 1)[0];
    };
    if (count == 2) {
        return Interpolation1D(nullptr, argIn[0], argIn[1], method, extrapolationMode,
            extrapolationValue, spline, result);
    }
    if (count == 3) {
        return Interpolation1D(&argIn[0], argIn[1], argIn[2], method, extrapolationMode,
            extrapolationValue, spline, result);
    }
    return false;
}
//=============================================================================
static bool
isKnownMethod(const std::wstring& method)
{
    return method == L"linear" || method == L"nearest" || method == L"previous" || method == L"next"
        || method == L"pchip" || method == L"spline" || method == L"cubic" || method == L"makima";
}
//=============================================================================
