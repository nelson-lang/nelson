//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nlsBuildConfig.h"
#include "lapack_eigen_config.hpp"
#include <Eigen/Dense>
#include <cmath>
#include <functional>
#include <limits>
#include "LogarithmGamma.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static double
gammalnd(double v)
{
    if ((v == 0) || (std::isinf(v) && (v > 0))) {
        return std::numeric_limits<double>::infinity();
    }
    if (v < 0) {
        Error(L"Nelson:gammaln:mustBeNonnegative", L"Input must be nonnegative.");
    }
    return std::lgamma(v);
}
//=============================================================================
static single
gammalns(single v)
{
    if ((v == 0) || (std::isinf(v) && (v > 0))) {
        return std::numeric_limits<single>::infinity();
    }
    if (v < 0) {
        Error(L"Nelson:gammaln:mustBeNonnegative", L"Input must be nonnegative.");
    }
    return std::lgamma(v);
}
//=============================================================================
ArrayOf
LogarithmGamma(const ArrayOf& arrayIn)
{
    ArrayOf res;
    if (!arrayIn.isEmpty()) {
        Dimensions dimsIn = arrayIn.dimensions();
        if (arrayIn.dataClass() == NLS_DOUBLE) {
            double* ptrOut = (double*)ArrayOf::allocateArrayOf(
                NLS_DOUBLE, dimsIn.elementCount(), stringVector(), false);
            auto* ptrIn = (double*)arrayIn.dataPointer();
            Eigen::Map<Eigen::ArrayXd> matOut(ptrOut, dimsIn.elementCount());
            Eigen::Map<Eigen::ArrayXd> matIn(ptrIn, dimsIn.elementCount());
            matOut = matIn.unaryExpr(std::ref(gammalnd));
            res = ArrayOf(NLS_DOUBLE, dimsIn, ptrOut);
        } else {
            single* ptrOut = (single*)ArrayOf::allocateArrayOf(
                NLS_SINGLE, dimsIn.elementCount(), stringVector(), false);
            auto* ptrIn = (single*)arrayIn.dataPointer();
            Eigen::Map<Eigen::ArrayXf> matOut(ptrOut, dimsIn.elementCount());
            Eigen::Map<Eigen::ArrayXf> matIn(ptrIn, dimsIn.elementCount());
            matOut = matIn.unaryExpr(std::ref(gammalns));
            res = ArrayOf(NLS_SINGLE, dimsIn, ptrOut);
        }
    } else {
        res = arrayIn;
        res.ensureSingleOwner();
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
