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
// Regularized lower incomplete gamma function P(x, a), computed with a series
// expansion for x < a + 1 and a continued fraction otherwise.
template <typename T>
static T
gammainc_lower_reg(T x, T a)
{
    const T EPS = std::numeric_limits<T>::epsilon();
    const T FPMIN = std::numeric_limits<T>::min() / EPS;
    if (a == (T)0) {
        return (x > (T)0) ? (T)1 : (T)0;
    }
    if (x == (T)0) {
        return (T)0;
    }
    if (x < a + (T)1) {
        T ap = a;
        T del = (T)1 / a;
        T sum = del;
        for (int n = 0; n < 1000; ++n) {
            ap += (T)1;
            del *= x / ap;
            sum += del;
            if (std::fabs(del) <= std::fabs(sum) * EPS) {
                break;
            }
        }
        return sum * std::exp(-x + a * std::log(x) - std::lgamma(a));
    }
    // continued fraction (Lentz) yields the upper Q(x, a); return 1 - Q
    T b = x + (T)1 - a;
    T c = (T)1 / FPMIN;
    T d = (T)1 / b;
    T h = d;
    for (int i = 1; i <= 1000; ++i) {
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
    T Q = std::exp(-x + a * std::log(x) - std::lgamma(a)) * h;
    return (T)1 - Q;
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
        T p = gammainc_lower_reg<T>(x, a);
        result[i] = isLower ? p : ((T)1 - p);
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
