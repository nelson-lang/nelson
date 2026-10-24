//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "odeErrorNormBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "i18n.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
//=============================================================================
using namespace Nelson;
//=============================================================================
static const double*
getRealDoubleVector(const ArrayOf& value, const std::wstring& message)
{
    if (value.dataClass() != NLS_DOUBLE || !value.isReal()) {
        Error(L"Nelson:ode:invalidArgument", message);
    }
    return static_cast<const double*>(value.dataPointer());
}
//=============================================================================
static void
checkSameElementCount(const ArrayOf& value, indexType n, const std::wstring& message)
{
    if (value.elementCount() != n) {
        Error(L"Nelson:ode:invalidArgument", message);
    }
}
//=============================================================================
ArrayOfVector
Nelson::OdeSolversGateway::odeErrorNormBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 6, 6);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval(1);
    const indexType n = argIn[0].elementCount();
    checkSameElementCount(argIn[1], n,
        resolveMessageTemplate(
            L"Nelson:ode_solvers:stateVectorsHaveSame", L"State vectors must have the same size."));
    checkSameElementCount(argIn[2], n,
        resolveMessageTemplate(
            L"Nelson:ode_solvers:stateVectorsHaveSame", L"State vectors must have the same size."));
    checkSameElementCount(argIn[3], n,
        Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:absoluteToleranceSizeDoes",
            L"Absolute tolerance size does not match state size."));
    const double* y = getRealDoubleVector(argIn[0],
        resolveMessageTemplate(
            L"Nelson:ode_solvers:realDoubleArray", L"Real double array expected."));
    const double* yhigh = getRealDoubleVector(argIn[1],
        resolveMessageTemplate(
            L"Nelson:ode_solvers:realDoubleArray", L"Real double array expected."));
    const double* ylow = getRealDoubleVector(argIn[2],
        resolveMessageTemplate(
            L"Nelson:ode_solvers:realDoubleArray", L"Real double array expected."));
    const double* absTol = getRealDoubleVector(argIn[3],
        resolveMessageTemplate(
            L"Nelson:ode_solvers:realDoubleArray", L"Real double array expected."));
    const double relTol = argIn[4].asDoubleScalar();
    const bool normControl = argIn[5].asDoubleScalar() != 0;
    double err = 0;
    double sumSquares = 0;
    for (indexType k = 0; k < n; ++k) {
        double scale = absTol[k] + std::max(std::abs(y[k]), std::abs(yhigh[k])) * relTol;
        // Guard against a zero denominator (zero absolute tolerance combined
        // with a zero state component).
        scale = std::max(scale, std::numeric_limits<double>::min());
        const double scaledError = std::abs(yhigh[k] - ylow[k]) / scale;
        if (normControl) {
            sumSquares += scaledError * scaledError;
        } else {
            err = std::max(err, scaledError);
        }
    }
    if (normControl) {
        err = n == 0 ? 0 : std::sqrt(sumSquares) / std::sqrt(static_cast<double>(n));
    }
    retval << ArrayOf::doubleScalar(err);
    return retval;
}
//=============================================================================
