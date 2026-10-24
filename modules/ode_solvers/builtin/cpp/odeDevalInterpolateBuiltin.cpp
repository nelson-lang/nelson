//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "odeDevalInterpolateBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "i18n.hpp"
#include "omp_for_loop.hpp"
#include <algorithm>
#include <cmath>
//=============================================================================
using namespace Nelson;
//=============================================================================
static const double*
getDoubleData(const ArrayOf& value, const std::wstring& message)
{
    if (value.dataClass() != NLS_DOUBLE || !value.isReal()) {
        Error(L"Nelson:deval:invalidArgument", message);
    }
    return static_cast<const double*>(value.dataPointer());
}
//=============================================================================
static indexType
findInterval(const double* t, indexType nt, double x, bool forward)
{
    if (nt < 2) {
        return 0;
    }
    if ((forward && x <= t[0]) || (!forward && x >= t[0])) {
        return 0;
    }
    if ((forward && x >= t[nt - 1]) || (!forward && x <= t[nt - 1])) {
        return nt - 2;
    }
    indexType lo = 0;
    indexType hi = nt - 1;
    while (hi - lo > 1) {
        indexType mid = lo + (hi - lo) / 2;
        if ((forward && t[mid] <= x) || (!forward && t[mid] >= x)) {
            lo = mid;
        } else {
            hi = mid;
        }
    }
    return lo;
}
//=============================================================================
static void
interpolateLinear(const double* values, double* y, double* yp, indexType rows, indexType idx,
    indexType point, double dt, double alpha)
{
    OMP_PARALLEL_FOR_LOOP(rows, 4)
    for (ompIndexType row = 0; row < static_cast<ompIndexType>(rows); ++row) {
        const indexType rowIndex = static_cast<indexType>(row);
        const double y0 = values[rowIndex + idx * rows];
        const double y1 = values[rowIndex + (idx + 1) * rows];
        if (dt == 0) {
            y[rowIndex + point * rows] = y0;
            yp[rowIndex + point * rows] = 0;
        } else {
            yp[rowIndex + point * rows] = (y1 - y0) / dt;
            y[rowIndex + point * rows] = y0 + alpha * (y1 - y0);
        }
    }
}
//=============================================================================
static void
interpolateHermite(const double* values, const double* slopes, double* y, double* yp,
    indexType rows, indexType idx, indexType point, double dt, double alpha)
{
    const double alpha2 = alpha * alpha;
    const double alpha3 = alpha2 * alpha;
    const double h00 = 2 * alpha3 - 3 * alpha2 + 1;
    const double h10 = alpha3 - 2 * alpha2 + alpha;
    const double h01 = -2 * alpha3 + 3 * alpha2;
    const double h11 = alpha3 - alpha2;
    const double dh00 = 6 * alpha2 - 6 * alpha;
    const double dh10 = 3 * alpha2 - 4 * alpha + 1;
    const double dh01 = -6 * alpha2 + 6 * alpha;
    const double dh11 = 3 * alpha2 - 2 * alpha;
    OMP_PARALLEL_FOR_LOOP(rows, 4)
    for (ompIndexType row = 0; row < static_cast<ompIndexType>(rows); ++row) {
        const indexType rowIndex = static_cast<indexType>(row);
        const double y0 = values[rowIndex + idx * rows];
        const double y1 = values[rowIndex + (idx + 1) * rows];
        const double yp0 = slopes[rowIndex + idx * rows];
        const double yp1 = slopes[rowIndex + (idx + 1) * rows];
        y[rowIndex + point * rows] = h00 * y0 + h10 * dt * yp0 + h01 * y1 + h11 * dt * yp1;
        if (dt == 0) {
            yp[rowIndex + point * rows] = 0;
        } else {
            yp[rowIndex + point * rows]
                = (dh00 * y0 + dh10 * dt * yp0 + dh01 * y1 + dh11 * dt * yp1) / dt;
        }
    }
}
//=============================================================================
ArrayOfVector
Nelson::OdeSolversGateway::odeDevalInterpolateBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 4, 4);
    nargoutcheck(nLhs, 0, 2);
    const double* t = getDoubleData(argIn[0],
        resolveMessageTemplate(
            L"Nelson:ode_solvers:realDoubleArray", L"Real double array expected."));
    const double* values = getDoubleData(argIn[1],
        resolveMessageTemplate(
            L"Nelson:ode_solvers:realDoubleArray", L"Real double array expected."));
    const double* x = getDoubleData(argIn[2],
        resolveMessageTemplate(
            L"Nelson:ode_solvers:realDoubleArray", L"Real double array expected."));
    const bool hasSlopes = !argIn[3].isEmpty();
    const double* slopes = hasSlopes
        ? getDoubleData(argIn[3],
              resolveMessageTemplate(
                  L"Nelson:ode_solvers:realDoubleArray", L"Real double array expected."))
        : nullptr;
    const indexType nt = argIn[0].elementCount();
    const indexType rows = argIn[1].rows();
    const indexType cols = argIn[1].columns();
    const indexType nx = argIn[2].elementCount();
    if (nt != cols) {
        Error(L"Nelson:dimagree:timeValuesSizesAreIncompatible",
            L"Time and values sizes are incompatible.");
    }
    if (hasSlopes && (argIn[3].rows() != rows || argIn[3].columns() < cols)) {
        Error(L"Nelson:dimagree:slopesSizeIncompatible", L"Slopes size is incompatible.");
    }
    double* y = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, rows * nx));
    // Wrap both buffers immediately so an unreturned one is still released.
    ArrayOf yArray(NLS_DOUBLE, Dimensions(rows, nx), y);
    double* yp = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, rows * nx));
    ArrayOf ypArray(NLS_DOUBLE, Dimensions(rows, nx), yp);
    if (nt == 0) {
        ArrayOfVector retval;
        retval << yArray;
        if (nLhs > 1) {
            retval << ypArray;
        }
        return retval;
    }
    const bool forward = t[nt - 1] >= t[0];
    for (indexType point = 0; point < nx; ++point) {
        indexType exact = 0;
        bool exactFound = false;
        for (indexType col = 0; col < nt; ++col) {
            if (t[col] == x[point]) {
                exact = col;
                exactFound = true;
                break;
            }
        }
        if (exactFound) {
            OMP_PARALLEL_FOR_LOOP(rows, 4)
            for (ompIndexType row = 0; row < static_cast<ompIndexType>(rows); ++row) {
                const indexType rowIndex = static_cast<indexType>(row);
                y[rowIndex + point * rows] = values[rowIndex + exact * rows];
                yp[rowIndex + point * rows] = hasSlopes ? slopes[rowIndex + exact * rows] : 0;
            }
            continue;
        }
        const indexType idx = findInterval(t, nt, x[point], forward);
        const double dt = t[idx + 1] - t[idx];
        const double alpha = dt == 0 ? 0 : (x[point] - t[idx]) / dt;
        if (hasSlopes) {
            interpolateHermite(values, slopes, y, yp, rows, idx, point, dt, alpha);
        } else {
            interpolateLinear(values, y, yp, rows, idx, point, dt, alpha);
        }
    }
    ArrayOfVector retval;
    retval << yArray;
    if (nLhs > 1) {
        retval << ypArray;
    }
    return retval;
}
//=============================================================================
