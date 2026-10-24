//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <cmath>
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "ArrayOf.hpp"
#include "complex_abs.hpp"
#include "BinaryOperatorsHelpers.hpp"
//=============================================================================
// Dense max/min kernels shared by Maximum.cpp and Minimum.cpp.
//
// Policy::better(a, b) is true when a strictly beats b, so ties always keep the
// first operand (or the first element met along the reduced dimension).
// Complex values are ordered by magnitude, then by phase angle.
// omitnan ignores NaN values (an all-NaN line gives NaN with index 1);
// includenan returns the first NaN met.
//=============================================================================
namespace Nelson {
//=============================================================================
struct MaxPolicy
{
    template <class T>
    static inline bool
    better(T a, T b)
    {
        return a > b;
    }
};
//=============================================================================
struct MinPolicy
{
    template <class T>
    static inline bool
    better(T a, T b)
    {
        return a < b;
    }
};
//=============================================================================
template <class Policy, class T>
static inline bool
complexBetter(T amag, T are, T aim, T bmag, T bre, T bim)
{
    if (amag != bmag) {
        return Policy::better(amag, bmag);
    }
    return Policy::better(std::atan2(aim, are), std::atan2(bim, bre));
}
//=============================================================================
template <class Policy, class T>
static inline T
extremumRealPair(bool omitnan, T x, T y)
{
    if (std::isnan(x)) {
        return omitnan ? y : x;
    }
    if (std::isnan(y)) {
        return omitnan ? x : y;
    }
    return Policy::better(y, x) ? y : x;
}
//=============================================================================
template <class Policy, class T>
static inline void
extremumComplexPair(bool omitnan, T xre, T xim, T yre, T yim, T* dst)
{
    T xmag = complex_abs(xre, xim);
    T ymag = complex_abs(yre, yim);
    bool pickY;
    if (std::isnan(xmag)) {
        pickY = omitnan && !std::isnan(ymag);
    } else if (std::isnan(ymag)) {
        pickY = !omitnan;
    } else {
        pickY = complexBetter<Policy, T>(ymag, yre, yim, xmag, xre, xim);
    }
    dst[0] = pickY ? yre : xre;
    dst[1] = pickY ? yim : xim;
}
//=============================================================================
template <class Policy, class T>
void
TExtremumLessInteger(
    const T* spx, const T* spy, T* dp, indexType count, indexType stridex, indexType stridey)
{
    OMP_PARALLEL_FOR_LOOP(count)
    for (ompIndexType i = 0; i < (ompIndexType)count; i++) {
        T x = spx[stridex * i];
        T y = spy[stridey * i];
        dp[i] = Policy::better(y, x) ? y : x;
    }
}
//=============================================================================
template <class Policy, class T>
void
TExtremumLessReal(bool omitnan, const T* spx, const T* spy, T* dp, indexType count,
    indexType stridex, indexType stridey)
{
    OMP_PARALLEL_FOR_LOOP(count)
    for (ompIndexType i = 0; i < (ompIndexType)count; i++) {
        dp[i] = extremumRealPair<Policy, T>(omitnan, spx[stridex * i], spy[stridey * i]);
    }
}
//=============================================================================
template <class Policy, class T>
void
TExtremumLessComplex(bool omitnan, const T* spx, const T* spy, T* dp, indexType count,
    indexType stridex, indexType stridey)
{
    OMP_PARALLEL_FOR_LOOP(count)
    for (ompIndexType i = 0; i < (ompIndexType)count; i++) {
        extremumComplexPair<Policy, T>(omitnan, spx[2 * stridex * i], spx[2 * stridex * i + 1],
            spy[2 * stridey * i], spy[2 * stridey * i + 1], &dp[2 * i]);
    }
}
//=============================================================================
template <class Policy, class T>
void
TExtremumBroadcastInteger(const T* spx, const T* spy, T* dp, const BinaryBroadcast& info)
{
    indexType count = info.dimensions.elementCount();
    OMP_PARALLEL_FOR_LOOP(count)
    for (ompIndexType i = 0; i < (ompIndexType)count; i++) {
        T x = spx[binaryOperatorBroadcastIndex(info, i, info.aStrides)];
        T y = spy[binaryOperatorBroadcastIndex(info, i, info.bStrides)];
        dp[i] = Policy::better(y, x) ? y : x;
    }
}
//=============================================================================
template <class Policy, class T>
void
TExtremumBroadcastReal(bool omitnan, const T* spx, const T* spy, T* dp, const BinaryBroadcast& info)
{
    indexType count = info.dimensions.elementCount();
    OMP_PARALLEL_FOR_LOOP(count)
    for (ompIndexType i = 0; i < (ompIndexType)count; i++) {
        T x = spx[binaryOperatorBroadcastIndex(info, i, info.aStrides)];
        T y = spy[binaryOperatorBroadcastIndex(info, i, info.bStrides)];
        dp[i] = extremumRealPair<Policy, T>(omitnan, x, y);
    }
}
//=============================================================================
template <class Policy, class T>
void
TExtremumBroadcastComplex(
    bool omitnan, const T* spx, const T* spy, T* dp, const BinaryBroadcast& info)
{
    indexType count = info.dimensions.elementCount();
    OMP_PARALLEL_FOR_LOOP(count)
    for (ompIndexType i = 0; i < (ompIndexType)count; i++) {
        indexType ia = binaryOperatorBroadcastIndex(info, i, info.aStrides);
        indexType ib = binaryOperatorBroadcastIndex(info, i, info.bStrides);
        extremumComplexPair<Policy, T>(
            omitnan, spx[2 * ia], spx[2 * ia + 1], spy[2 * ib], spy[2 * ib + 1], &dp[2 * i]);
    }
}
//=============================================================================
// reduction of one line of `linesize` elements spaced by `stride`
template <class Policy, class T>
static inline void
extremumLineInteger(const T* sp, indexType linesize, indexType stride, T& value, double& index)
{
    T best = sp[0];
    double bestIndex = 0;
    for (ompIndexType k = 1; k < (ompIndexType)linesize; k++) {
        T val = sp[k * stride];
        if (Policy::better(val, best)) {
            best = val;
            bestIndex = (double)k;
        }
    }
    value = best;
    index = bestIndex + 1;
}
//=============================================================================
template <class Policy, class T>
static inline void
extremumLineReal(
    bool omitnan, const T* sp, indexType linesize, indexType stride, T& value, double& index)
{
    // signed loop counter and floating-point index: the compiler turns this loop
    // into compare/blend code (an integer index makes it 3 times slower)
    if (omitnan) {
        indexType first = 0;
        while (first < linesize && std::isnan(sp[first * stride])) {
            first++;
        }
        if (first == linesize) {
            // only NaN values
            value = sp[0];
            index = 1;
            return;
        }
        T best = sp[first * stride];
        double bestIndex = (double)first;
        for (ompIndexType k = first + 1; k < (ompIndexType)linesize; k++) {
            T val = sp[k * stride];
            if (!std::isnan(val)) {
                if (Policy::better(val, best)) {
                    best = val;
                    bestIndex = (double)k;
                }
            }
        }
        value = best;
        index = bestIndex + 1;
    } else {
        // the first NaN met wins over any value
        T best = sp[0];
        double bestIndex = 0;
        for (ompIndexType k = 0; k < (ompIndexType)linesize; k++) {
            T val = sp[k * stride];
            if (Policy::better(val, best) || (std::isnan(val) && !std::isnan(best))) {
                best = val;
                bestIndex = (double)k;
            }
        }
        value = best;
        index = bestIndex + 1;
    }
}
//=============================================================================
// same reduction without the index: select-style updates that the compiler
// vectorises (a branchy value-only loop is 3 times slower)
template <class Policy, class T>
static inline void
extremumLineRealValue(bool omitnan, const T* sp, indexType linesize, indexType stride, T& value)
{
    T best = sp[0];
    if (omitnan) {
        for (ompIndexType k = 1; k < (ompIndexType)linesize; k++) {
            T val = sp[k * stride];
            best = (Policy::better(val, best) || (std::isnan(best) && !std::isnan(val))) ? val
                                                                                         : best;
        }
    } else {
        for (ompIndexType k = 1; k < (ompIndexType)linesize; k++) {
            T val = sp[k * stride];
            best = (Policy::better(val, best) || (std::isnan(val) && !std::isnan(best))) ? val
                                                                                         : best;
        }
    }
    value = best;
}
//=============================================================================
template <class Policy, class T>
static inline void
extremumLineComplex(
    bool omitnan, const T* sp, indexType linesize, indexType stride, T* value, double& index)
{
    indexType bestIndex = 0;
    T bestRe = sp[0];
    T bestIm = sp[1];
    T bestMag = complex_abs(bestRe, bestIm);
    indexType k = 0;
    if (omitnan) {
        while (
            k < linesize && std::isnan(complex_abs(sp[2 * k * stride], sp[2 * k * stride + 1]))) {
            k++;
        }
        if (k < linesize) {
            bestRe = sp[2 * k * stride];
            bestIm = sp[2 * k * stride + 1];
            bestMag = complex_abs(bestRe, bestIm);
            bestIndex = k;
            k++;
        }
    } else if (!std::isnan(bestMag)) {
        k = 1;
    } else {
        k = linesize;
    }
    for (; k < linesize; k++) {
        T re = sp[2 * k * stride];
        T im = sp[2 * k * stride + 1];
        T mag = complex_abs(re, im);
        if (std::isnan(mag)) {
            if (omitnan) {
                continue;
            }
            bestRe = re;
            bestIm = im;
            bestIndex = k;
            break;
        }
        if (complexBetter<Policy, T>(mag, re, im, bestMag, bestRe, bestIm)) {
            bestMag = mag;
            bestRe = re;
            bestIm = im;
            bestIndex = k;
        }
    }
    value[0] = bestRe;
    value[1] = bestIm;
    index = (double)(bestIndex + 1);
}
//=============================================================================
template <class Policy, class T>
void
TExtremumAllInteger(const T* sp, T* dp, indexType elementCount)
{
    double index = 0;
    extremumLineInteger<Policy, T>(sp, elementCount, 1, dp[0], index);
}
//=============================================================================
template <class Policy, class T>
void
TExtremumAllReal(bool omitnan, const T* sp, T* dp, indexType elementCount)
{
    extremumLineRealValue<Policy, T>(omitnan, sp, elementCount, 1, dp[0]);
}
//=============================================================================
template <class Policy, class T>
void
TExtremumAllComplex(bool omitnan, const T* sp, T* dp, indexType elementCount)
{
    double index = 0;
    extremumLineComplex<Policy, T>(omitnan, sp, elementCount, 1, dp, index);
}
//=============================================================================
// reduction along one dimension: the array is seen as `planes` blocks of
// `planesize` lines of `linesize` elements (elements of a line are `planesize` apart)
template <class Policy, class T>
void
TExtremumInteger(
    const T* sp, T* dp, double* iptr, indexType planes, indexType planesize, indexType linesize)
{
    ompIndexType lines = (ompIndexType)(planes * planesize);
    for (ompIndexType line = 0; line < lines; line++) {
        indexType i = line / planesize;
        indexType j = line % planesize;
        double index = 0;
        extremumLineInteger<Policy, T>(
            sp + i * planesize * linesize + j, linesize, planesize, dp[line], index);
        if (iptr != nullptr) {
            iptr[line] = index;
        }
    }
}
//=============================================================================
template <class Policy, class T>
void
TExtremumReal(bool omitnan, const T* sp, T* dp, double* iptr, indexType planes, indexType planesize,
    indexType linesize)
{
    ompIndexType lines = (ompIndexType)(planes * planesize);
    for (ompIndexType line = 0; line < lines; line++) {
        indexType i = line / planesize;
        indexType j = line % planesize;
        const T* firstElement = sp + i * planesize * linesize + j;
        if (iptr == nullptr) {
            extremumLineRealValue<Policy, T>(omitnan, firstElement, linesize, planesize, dp[line]);
        } else {
            extremumLineReal<Policy, T>(
                omitnan, firstElement, linesize, planesize, dp[line], iptr[line]);
        }
    }
}
//=============================================================================
template <class Policy, class T>
void
TExtremumComplex(bool omitnan, const T* sp, T* dp, double* iptr, indexType planes,
    indexType planesize, indexType linesize)
{
    ompIndexType lines = (ompIndexType)(planes * planesize);
    for (ompIndexType line = 0; line < lines; line++) {
        indexType i = line / planesize;
        indexType j = line % planesize;
        double index = 0;
        extremumLineComplex<Policy, T>(omitnan, sp + 2 * (i * planesize * linesize + j), linesize,
            planesize, dp + 2 * line, index);
        if (iptr != nullptr) {
            iptr[line] = index;
        }
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
