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
#include <algorithm>
#include <limits>
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "GammaIncomplete.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
inline Dimensions
maxDimensions(Dimensions& a, Dimensions& b)
{
    Dimensions ret;
    size_t len = std::max(a.length(), b.length());
    for (size_t i = 0; i < len; i++) {
        indexType da = (i < a.length()) ? a.at(i, false) : 1;
        indexType db = (i < b.length()) ? b.at(i, false) : 1;
        ret[i] = (da > db) ? da : db;
    }
    return ret;
}
//=============================================================================
// log(x^a * exp(-x) / gamma(a)), with a Stirling form for large a that avoids cancellation
template <typename T>
static T
gammaincLogPrefactor(T x, T a)
{
    if (a < (T)100) {
        return -x + a * std::log(x) - std::lgamma(a);
    }
    const T d = (x - a) / a;
    const T inv = (T)1 / a;
    const T inv2 = inv * inv;
    const T correction
        = inv * ((T)1 / 12 - inv2 * ((T)1 / 360 - inv2 * ((T)1 / 1260 - inv2 / 1680)));
    return a * (std::log1p(d) - d) + (T)0.5 * std::log(a / (T)6.283185307179586477) - correction;
}
//=============================================================================
// Regularized incomplete gamma functions P(x, a) and Q(x, a): series for x < a + 1,
// continued fraction otherwise, so the small tail never comes from 1 - (1 - tail).
template <typename T>
static void
gammainc_reg(T x, T a, T& P, T& Q)
{
    const T EPS = std::numeric_limits<T>::epsilon();
    const T FPMIN = std::numeric_limits<T>::min() / EPS;
    if (std::isnan(x) || std::isnan(a)) {
        P = Q = std::numeric_limits<T>::quiet_NaN();
        return;
    }
    // the lower tail tends to 1 when x grows and to 0 when a grows
    if ((a == (T)0 && x > (T)0) || std::isinf(x)) {
        P = (T)1;
        Q = (T)0;
        return;
    }
    if (x == (T)0 || std::isinf(a)) {
        P = (T)0;
        Q = (T)1;
        return;
    }
    const T logPrefactor = gammaincLogPrefactor(x, a);
    // both expansions need about sqrt(a) terms near x = a
    const int maxTerms = 1000 + (int)std::min((T)1e8, (T)20 * std::sqrt(a));
    if (x < a + (T)1) {
        T ap = a;
        T del = (T)1 / a;
        T sum = del;
        for (int n = 0; n < maxTerms; ++n) {
            ap += (T)1;
            del *= x / ap;
            sum += del;
            if (std::fabs(del) <= std::fabs(sum) * EPS) {
                break;
            }
        }
        P = std::exp(logPrefactor + std::log(sum));
        Q = (T)1 - P;
        return;
    }
    // continued fraction (Lentz) for Q
    T b = x + (T)1 - a;
    T c = (T)1 / FPMIN;
    T d = (T)1 / b;
    T h = d;
    for (int i = 1; i <= maxTerms; ++i) {
        T an = (T)(-i) * ((T)i - a);
        b += (T)2;
        d = an * d + b;
        if (std::fabs(d) < FPMIN) {
            d = FPMIN;
        }
        c = b + an / c;
        if (std::fabs(c) < FPMIN) {
            c = FPMIN;
        }
        d = (T)1 / d;
        T del = d * c;
        h *= del;
        if (std::fabs(del - (T)1) <= EPS) {
            break;
        }
    }
    Q = std::exp(logPrefactor + std::log(h));
    P = (T)1 - Q;
}
//=============================================================================
template <class T>
static ArrayOf
GammaIncomplete(const Dimensions& retDims, NelsonType destinationType, indexType maxLen,
    const ArrayOf& X, const ArrayOf& A, bool isLower)
{
    T* result = (T*)ArrayOf::allocateArrayOf(
        destinationType, retDims.elementCount(), stringVector(), false);
    ArrayOf res = ArrayOf(destinationType, retDims, result);
    auto* ptrX = (T*)X.dataPointer();
    auto* ptrA = (T*)A.dataPointer();
    OMP_PARALLEL_FOR_LOOP(maxLen)
    for (ompIndexType i = 0; i < (ompIndexType)maxLen; ++i) {
        T x = (X.isScalar()) ? ptrX[0] : ptrX[i];
        T a = (A.isScalar()) ? ptrA[0] : ptrA[i];
        T P, Q;
        gammainc_reg<T>(x, a, P, Q);
        result[i] = isLower ? P : Q;
    }
    return res;
}
//=============================================================================
template <class T>
static bool
allNonnegative(const ArrayOf& V)
{
    auto* ptr = (T*)V.dataPointer();
    indexType n = V.elementCount();
    for (indexType i = 0; i < n; ++i) {
        if (!(ptr[i] >= (T)0)) {
            return false;
        }
    }
    return true;
}
//=============================================================================
ArrayOf
GammaIncomplete(const ArrayOf& X, const ArrayOf& A, bool isLower, bool& needToOverload)
{
    needToOverload = false;
    ArrayOf res;
    bool okX = (X.dataClass() == NLS_DOUBLE || X.dataClass() == NLS_SINGLE) && !X.isSparse();
    bool okA = (A.dataClass() == NLS_DOUBLE || A.dataClass() == NLS_SINGLE) && !A.isSparse();
    if (!okX || !okA) {
        needToOverload = true;
        return res;
    }
    Dimensions dimsX = X.dimensions();
    Dimensions dimsA = A.dimensions();
    Dimensions retDims = maxDimensions(dimsX, dimsA);
    if (!X.isScalar() && !retDims.equals(dimsX)) {
        Error("Nelson:dimagree:xASizeEitherCanA",
            "X and A must be the same size, or either can be a scalar.");
    }
    if (!A.isScalar() && !retDims.equals(dimsA)) {
        Error("Nelson:dimagree:xASizeEitherCanA",
            "X and A must be the same size, or either can be a scalar.");
    }
    indexType maxLen = std::max(X.elementCount(), A.elementCount());
    bool useSingle = (X.dataClass() == NLS_SINGLE) || (A.dataClass() == NLS_SINGLE);
    if (useSingle) {
        ArrayOf XX(X);
        ArrayOf AA(A);
        XX.promoteType(NLS_SINGLE);
        AA.promoteType(NLS_SINGLE);
        if (!allNonnegative<single>(XX) || !allNonnegative<single>(AA)) {
            Error("Nelson:gammainc:mustBeNonnegative", "X and A must contain nonnegative values.");
        }
        res = GammaIncomplete<single>(retDims, NLS_SINGLE, maxLen, XX, AA, isLower);
    } else {
        ArrayOf XX(X);
        ArrayOf AA(A);
        XX.promoteType(NLS_DOUBLE);
        AA.promoteType(NLS_DOUBLE);
        if (!allNonnegative<double>(XX) || !allNonnegative<double>(AA)) {
            Error("Nelson:gammainc:mustBeNonnegative", "X and A must contain nonnegative values.");
        }
        res = GammaIncomplete<double>(retDims, NLS_DOUBLE, maxLen, XX, AA, isLower);
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
