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
#include "stdBuiltin.hpp"
#include "OverloadRequired.hpp"
#include "VarianceOptions.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static ArrayOf
stdFromVariance(const ArrayOf& variance, bool& needToOverload);
//=============================================================================
ArrayOfVector
Nelson::StatisticsGateway::stdBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 4);
    nargoutcheck(nLhs, 0, 2);
    VarianceOptions options = ParseVarianceOptions(argIn);
    bool sparseOutput = argIn[0].isSparse();
    ArrayOf x = PrepareVarianceInput(argIn[0]);
    bool needToOverload = false;
    ArrayOf variance = VarianceWithOptions(x, options, needToOverload);
    if (needToOverload) {
        OverloadRequired("std");
    }
    ArrayOf sigma = stdFromVariance(variance, needToOverload);
    if (needToOverload) {
        OverloadRequired("std");
    }
    if (sparseOutput) {
        sigma.makeSparse();
    }
    retval << sigma;
    if (nLhs > 1) {
        ArrayOf meanValue = VarianceMean(x, options, needToOverload);
        if (needToOverload) {
            OverloadRequired("std");
        }
        if (sparseOutput) {
            meanValue.makeSparse();
        }
        retval << meanValue;
    }
    return retval;
}
//=============================================================================
template <class T>
static ArrayOf
sqrtRealArray(const ArrayOf& variance, NelsonType realClass)
{
    Dimensions dims = variance.dimensions();
    indexType n = variance.elementCount();
    const T* src = (const T*)variance.dataPointer();
    T* dst = (T*)ArrayOf::allocateArrayOf(realClass, n);
    for (indexType k = 0; k < n; ++k) {
        dst[k] = (T)std::sqrt(src[k]);
    }
    return ArrayOf(realClass, dims, dst);
}
//=============================================================================
static ArrayOf
stdFromVariance(const ArrayOf& variance, bool& needToOverload)
{
    needToOverload = false;
    switch (variance.dataClass()) {
    case NLS_DOUBLE:
        return sqrtRealArray<double>(variance, NLS_DOUBLE);
    case NLS_SINGLE:
        return sqrtRealArray<single>(variance, NLS_SINGLE);
    default:
        needToOverload = true;
        return {};
    }
}
//=============================================================================
