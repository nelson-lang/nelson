//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#if defined(__GNUC__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpsabi"
#endif
#include <cmath>
#include <algorithm>
#include <limits>
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "lapack_eigen_config.hpp"
#include <Eigen/Dense>
#include "BetaIncomplete.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
inline Dimensions
maxDimensions(Dimensions& a, Dimensions& b)
{
    Dimensions ret;
    for (unsigned int i = 0; i < std::min(a.getLength(), b.getLength()); i++) {
        ret[i] = (a.getAt(i, false) > b.getAt(i, false)) ? a.getAt(i, false) : b.getAt(i, false);
    }
    return ret;
}
//=============================================================================
// Implementation of the regularized incomplete beta function I_x(a,b)
// using continued fraction
template <typename T>
static T
betacf(T a, T b, T x)
{
    const int MAXIT = 200;
    const T EPS = std::numeric_limits<T>::epsilon();
    const T FPMIN = std::numeric_limits<T>::min() / EPS;

    T qab = a + b;
    T qap = a + (T)1.0;
    T qam = a - (T)1.0;
    T c = (T)1.0;
    T d = (T)1.0 - qab * x / qap;
    if (std::fabs(d) < FPMIN) {
        d = FPMIN;
    }
    d = (T)1.0 / d;
    T h = d;

    for (int m = 1; m <= MAXIT; ++m) {
        int m2 = 2 * m;
        // even step
        T aa = (T)m * (b - (T)m) * x / ((qam + (T)m2) * (a + (T)m2));
        d = (T)1.0 + aa * d;
        if (std::fabs(d) < FPMIN) {
            d = FPMIN;
        }
        c = (T)1.0 + aa / c;
        if (std::fabs(c) < FPMIN) {
            c = FPMIN;
        }
        d = (T)1.0 / d;
        h *= d * c;

        // odd step
        aa = -(a + (T)m) * (qab + (T)m) * x / ((a + (T)m2) * (qap + (T)m2));
        d = (T)1.0 + aa * d;
        if (std::fabs(d) < FPMIN) {
            d = FPMIN;
        }
        c = (T)1.0 + aa / c;
        if (std::fabs(c) < FPMIN) {
            c = FPMIN;
        }
        d = (T)1.0 / d;
        T del = d * c;
        h *= del;

        if (std::fabs(del - (T)1.0) < (T)10 * EPS) {
            return h;
        }
    }
    // if we reach here, the continued fraction failed to converge
    return h;
}
//=============================================================================
template <typename T>
static T
ibeta(T a, T b, T x)
{
    // Regularized incomplete beta function I_x(a,b)
    if (x <= (T)0.0) {
        return (T)0.0;
    }
    if (x >= (T)1.0) {
        return (T)1.0;
    }

    // Handle infinite parameters to avoid NaN from lgamma(inf)-lgamma(inf)
    if (std::isinf(a) && a > 0) {
        // a -> +inf: distribution concentrates near x=1, so I_x -> 0 for x in [0,1)
        return (x < (T)1.0) ? (T)0.0 : (T)1.0;
    }
    if (std::isinf(b) && b > 0) {
        // b -> +inf: distribution concentrates near x=0, so I_x -> 1 for x in (0,1]
        return (x > (T)0.0) ? (T)1.0 : (T)0.0;
    }

    // Compute ln(beta(a,b)) via lgamma
    T ln_beta = std::lgamma(a) + std::lgamma(b) - std::lgamma(a + b);

    // Use symmetry transformation if necessary for better convergence
    T front;
    if (x < (a + (T)1.0) / (a + b + (T)2.0)) {
        // use continued fraction directly
        front = std::exp(a * std::log(x) + b * std::log((T)1.0 - x) - ln_beta) / a;
        T cf = betacf<T>(a, b, x);
        return front * cf;
    } else {
        // Use symmetry relation I_x(a,b) = 1 - I_{1-x}(b,a)
        front = std::exp(b * std::log((T)1.0 - x) + a * std::log(x) - ln_beta) / b;
        T cf = betacf<T>(b, a, (T)1.0 - x);
        T result = front * cf;
        return (T)1.0 - result;
    }
}
//=============================================================================
template <class T>
ArrayOf
BetaIncomplete(const Dimensions& retDims, NelsonType destinationType, indexType maxLen,
    const ArrayOf& X, const ArrayOf& Y, const ArrayOf& Z, bool isLower)
{
    T* result = (T*)ArrayOf::allocateArrayOf(
        destinationType, retDims.getElementCount(), stringVector(), false);
    ArrayOf res = ArrayOf(destinationType, retDims, result);
    auto* ptrX = (T*)X.getDataPointer();
    auto* ptrY = (T*)Y.getDataPointer();
    auto* ptrZ = (T*)Z.getDataPointer();
    if (isLower) {
        OMP_PARALLEL_FOR_LOOP(maxLen)
        for (ompIndexType i = 0; i < (ompIndexType)maxLen; ++i) {
            T x = (X.isScalar()) ? ptrX[0] : ptrX[i];
            T y = (Y.isScalar()) ? ptrY[0] : ptrY[i];
            T z = (Z.isScalar()) ? ptrZ[0] : ptrZ[i];
            try {
                result[i] = ibeta<T>(y, z, x);
            } catch (...) {
                result[i] = (T)1.0;
            }
        }
    } else {
        OMP_PARALLEL_FOR_LOOP(maxLen)
        for (ompIndexType i = 0; i < (ompIndexType)maxLen; ++i) {
            T x = (X.isScalar()) ? ptrX[0] : ptrX[i];
            T y = (Y.isScalar()) ? ptrY[0] : ptrY[i];
            T z = (Z.isScalar()) ? ptrZ[0] : ptrZ[i];
            try {
                result[i] = (T)1.0 - ibeta<T>(y, z, x);
            } catch (...) {
                result[i] = (T)0.0;
            }
        }
    }
    return res;
}
//=============================================================================
ArrayOf
BetaIncomplete(
    const ArrayOf& X, const ArrayOf& Y, const ArrayOf& Z, bool isLower, bool& needOverload)
{
    ArrayOf res;
    if (X.getDataClass() == NLS_DOUBLE || X.getDataClass() == NLS_SINGLE && !X.isSparse()) {
        needOverload = false;
    } else {
        needOverload = true;
        return res;
    }
    if (Y.getDataClass() == NLS_DOUBLE || Y.getDataClass() == NLS_SINGLE && !Y.isSparse()) {
        needOverload = false;
    } else {
        needOverload = true;
        return res;
    }
    if (Z.getDataClass() == NLS_DOUBLE || Z.getDataClass() == NLS_SINGLE && !Z.isSparse()) {
        needOverload = false;
    } else {
        needOverload = true;
        return res;
    }
    indexType maxLen
        = std::max(X.getElementCount(), std::max(Y.getElementCount(), Z.getElementCount()));
    Dimensions dimsX = X.getDimensions();
    Dimensions dimsY = Y.getDimensions();
    Dimensions dimsZ = Z.getDimensions();
    Dimensions dimsMaxYZ = maxDimensions(dimsY, dimsZ);
    Dimensions retDims = maxDimensions(dimsX, dimsMaxYZ);
    if (!(X.isScalar()) && !retDims.equals(X.getDimensions())) {
        Error(_("Wrong size for #1 argument."));
    }
    if (!(Y.isScalar()) && !retDims.equals(Y.getDimensions())) {
        Error(_("Wrong size for #2 argument."));
    }
    if (!(Z.isScalar()) && !retDims.equals(Z.getDimensions())) {
        Error(_("Wrong size for #3 argument."));
    }
    if (!Y.isPositive()) {
        Error(_("Wrong value for #2 argument. positive value expected."));
    }
    if (!Z.isPositive()) {
        Error(_("Wrong value for #3 argument. positive value expected."));
    }
    if (X.getDataClass() == NLS_DOUBLE) {
        ArrayOf YY(Y);
        ArrayOf ZZ(Z);
        YY.promoteType(NLS_DOUBLE);
        ZZ.promoteType(NLS_DOUBLE);

        Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic>> matX(
            (double*)X.getDataPointer(), 1, dimsX.getElementCount());
        bool isBetween0and1 = ((matX.array() >= 0).all() && (matX.array() <= 1).all());
        if (!isBetween0and1) {
            Error(_("Wrong value for #1 argument. [0, 1] values expected."));
        }
        res = BetaIncomplete<double>(retDims, NLS_DOUBLE, maxLen, X, YY, ZZ, isLower);
    } else if (X.getDataClass() == NLS_SINGLE) {
        ArrayOf YY(Y);
        ArrayOf ZZ(Z);
        YY.promoteType(NLS_SINGLE);
        ZZ.promoteType(NLS_SINGLE);

        Eigen::Map<Eigen::Matrix<single, Eigen::Dynamic, Eigen::Dynamic>> matX(
            (single*)X.getDataPointer(), 1, dimsX.getElementCount());
        bool isBetween0and1 = ((matX.array() >= 0).all() && (matX.array() <= 1).all());
        if (!isBetween0and1) {
            Error(_("Wrong value for #1 argument. [0, 1] values expected."));
        }
        res = BetaIncomplete<single>(retDims, NLS_SINGLE, maxLen, X, YY, ZZ, isLower);
    } else {
        needOverload = true;
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================

#if defined(__GNUC__)
#pragma GCC diagnostic pop
#endif
