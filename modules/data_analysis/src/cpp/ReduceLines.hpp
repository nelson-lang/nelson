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
#include <algorithm>
#include <limits>
#include <type_traits>
#include <vector>
#include "nlsBuildConfig.h"
#include "ArrayOf.hpp"
#include "IntegerOperations.hpp"
#include "NelsonConfiguration.hpp"
#include "omp_for_loop.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Interleaved complex element (real part then imaginary part).
template <class T> struct ComplexPair
{
    T re = 0;
    T im = 0;
};
//=============================================================================
// Array seen as planecount x linesize x planesize, lines running over the reduced dimensions.
struct ReductionLines
{
    Dimensions dimsRes;
    indexType planecount = 1;
    indexType planesize = 1;
    indexType linesize = 1;
};
//=============================================================================
// Geometry of a reduction over the consecutive dimensions first..last (0-based, inclusive).
static ReductionLines
reductionLines(const Dimensions& dimsA, indexType first, indexType last)
{
    ReductionLines g;
    g.dimsRes = dimsA;
    const indexType nd = dimsA.length();
    for (indexType l = 0; l < nd; l++) {
        const indexType len = dimsA.at(l);
        if (l < first) {
            g.planesize *= len;
        } else if (l <= last) {
            g.linesize *= len;
            g.dimsRes.setDimensionLength(l, 1);
        } else {
            g.planecount *= len;
        }
    }
    g.dimsRes.simplify();
    return g;
}
//=============================================================================
// Reduces each line of a planes x linesize x planesize array with step, in index order.
// Lines with planesize > 1 are handled by blocks of neighbours so rows are read contiguously.
template <class T, class OutT, class Acc, class StepFn>
static void
reduceLinesInOrder(const T* sp, OutT* dp, indexType planes, indexType planesize, indexType linesize,
    Acc init, StepFn step)
{
    constexpr ompIndexType blockSize = 256;
    const ompIndexType work = (ompIndexType)(planes * planesize * linesize);
    if (planesize == 1 && planes == 1) {
        // a single line stays outside any parallel region
        Acc accum = init;
        for (indexType k = 0; k < linesize; k++) {
            accum = step(accum, sp[k]);
        }
        dp[0] = (OutT)accum;
        return;
    }
    if (planesize == 1) {
        OMP_PARALLEL_FOR_LOOP(planes > 1 ? work : 0)
        for (ompIndexType p = 0; p < (ompIndexType)planes; p++) {
            const T* line = sp + p * linesize;
            Acc accum = init;
            for (indexType k = 0; k < linesize; k++) {
                accum = step(accum, line[k]);
            }
            dp[p] = (OutT)accum;
        }
        return;
    }
    const ompIndexType blocksPerPlane = ((ompIndexType)planesize + blockSize - 1) / blockSize;
    const ompIndexType blockCount = (ompIndexType)planes * blocksPerPlane;
    OMP_PARALLEL_FOR_LOOP(blockCount > 1 ? work : 0)
    for (ompIndexType b = 0; b < blockCount; b++) {
        const ompIndexType p = b / blocksPerPlane;
        const ompIndexType j0 = (b % blocksPerPlane) * blockSize;
        const ompIndexType n = std::min(blockSize, (ompIndexType)planesize - j0);
        Acc accum[blockSize];
        for (ompIndexType j = 0; j < n; j++) {
            accum[j] = init;
        }
        const T* first = sp + p * planesize * linesize + j0;
        for (indexType k = 0; k < linesize; k++) {
            const T* row = first + k * planesize;
            for (ompIndexType j = 0; j < n; j++) {
                accum[j] = step(accum[j], row[j]);
            }
        }
        OutT* out = dp + p * planesize + j0;
        for (ompIndexType j = 0; j < n; j++) {
            out[j] = (OutT)accum[j];
        }
    }
}
//=============================================================================
constexpr ompIndexType nativeSumChunk = 1 << 16;
//=============================================================================
// Saturating sum of one long line. Saturating adds compose into x -> clamp(x + s, a, b):
// chunks compute (s, a, b) in parallel, then are chained in order (rescan if s overflows).
template <class T, class Acc, class StepFn>
static T
nativeSumLongLine(const T* sp, indexType n, Acc lo, Acc hi, StepFn step)
{
    struct Chunk
    {
        Acc s;
        Acc a;
        Acc b;
        bool exact;
    };
    const ompIndexType count = ((ompIndexType)n + nativeSumChunk - 1) / nativeSumChunk;
    std::vector<Chunk> chunks((size_t)count);
    OMP_PARALLEL_FOR_LOOP((ompIndexType)n)
    for (ompIndexType c = 0; c < count; c++) {
        const ompIndexType begin = c * nativeSumChunk;
        const ompIndexType end = std::min(begin + nativeSumChunk, (ompIndexType)n);
        Acc s = 0;
        Acc a = lo;
        Acc b = hi;
        bool overflow = false;
        for (ompIndexType k = begin; k < end; k++) {
            if constexpr (sizeof(T) < 8) {
                s += (Acc)sp[k];
            } else if constexpr (std::is_signed<T>::value) {
                T r = (T)((uint64)s + (uint64)sp[k]);
                overflow |= ((s ^ r) & (sp[k] ^ r)) < 0;
                s = r;
            } else {
                T r = s + sp[k];
                overflow |= r < s;
                s = r;
            }
            a = step(a, sp[k]);
            b = step(b, sp[k]);
        }
        chunks[c] = { s, a, b, !overflow };
    }
    Acc acc = 0;
    for (ompIndexType c = 0; c < count; c++) {
        const Chunk& ch = chunks[c];
        if (ch.exact) {
            Acc shifted;
            if constexpr (sizeof(T) < 8) {
                shifted = acc + ch.s;
            } else {
                shifted = scalar_scalar_integer_addition<T>(acc, ch.s);
            }
            acc = std::min(std::max(shifted, ch.a), ch.b);
        } else {
            const ompIndexType end = std::min((c + 1) * nativeSumChunk, (ompIndexType)n);
            for (ompIndexType k = c * nativeSumChunk; k < end; k++) {
                acc = step(acc, sp[k]);
            }
        }
    }
    return (T)acc;
}
//=============================================================================
// Saturating product of one long line: once saturated it only depends on zeros, signs and +-1,
// so chunks are summarized in parallel and the summaries are chained in order.
template <class T, class Acc, class StepFn>
static T
nativeProdLongLine(const T* sp, indexType n, Acc lo, Acc hi, StepFn step)
{
    constexpr bool isSigned = std::is_signed<T>::value;
    auto saturated = [&](Acc v) { return v == hi || (isSigned && (v == lo || v == (Acc)(-hi))); };
    Acc acc = 1;
    ompIndexType k = 0;
    while (k < (ompIndexType)n && acc != 0 && !saturated(acc)) {
        acc = step(acc, sp[k++]);
    }
    const ompIndexType rest = (ompIndexType)n - k;
    if (acc == 0 || rest < 2 * nativeSumChunk) {
        for (; k < (ompIndexType)n && acc != 0; k++) {
            acc = step(acc, sp[k]);
        }
        return (T)acc;
    }
    struct Chunk
    {
        bool zero = false;
        bool big = false; // a factor with magnitude >= 2
        bool negBeforeBig = false; // odd count of negative factors before the last big one
        bool bigNegative = false;
        ompIndexType minusOnes = 0; // -1 factors after the last big one
    };
    const T* tail = sp + k;
    const ompIndexType count = (rest + nativeSumChunk - 1) / nativeSumChunk;
    std::vector<Chunk> chunks((size_t)count);
    OMP_PARALLEL_FOR_LOOP(rest)
    for (ompIndexType c = 0; c < count; c++) {
        const T* first = tail + c * nativeSumChunk;
        const ompIndexType len = std::min(nativeSumChunk, rest - c * nativeSumChunk);
        Chunk ch;
        // counting loops without early exit so they vectorize
        ompIndexType zeros = 0;
        for (ompIndexType j = 0; j < len; j++) {
            zeros += (first[j] == 0);
        }
        ch.zero = zeros > 0;
        if (!ch.zero) {
            // last factor of magnitude >= 2, searched from the end
            ompIndexType last = len - 1;
            while (last >= 0 && (first[last] == 1 || (isSigned && first[last] == (T)-1))) {
                last--;
            }
            ompIndexType negativesBefore = 0;
            ompIndexType negativesAfter = 0;
            if constexpr (isSigned) {
                for (ompIndexType j = 0; j < std::max(last, (ompIndexType)0); j++) {
                    negativesBefore += (first[j] < 0);
                }
                for (ompIndexType j = last + 1; j < len; j++) {
                    negativesAfter += (first[j] < 0);
                }
            }
            ch.big = last >= 0;
            ch.negBeforeBig = (negativesBefore % 2) == 1;
            ch.bigNegative = ch.big && isSigned && first[last] < 0;
            ch.minusOnes = negativesAfter;
        }
        chunks[c] = ch;
    }
    // a saturated product times -1: max -> -max, min -> max, -max -> max
    auto negate = [&](Acc v) { return v == hi ? (Acc)(-hi) : hi; };
    for (ompIndexType c = 0; c < count && acc != 0; c++) {
        const Chunk& ch = chunks[c];
        if (ch.zero) {
            acc = 0;
            break;
        }
        if (ch.big) {
            bool negativeBefore = isSigned && ((acc < 0) != ch.negBeforeBig);
            acc = (negativeBefore != ch.bigNegative) ? lo : hi;
        }
        if (ch.minusOnes > 0) {
            acc = negate(acc);
            if ((ch.minusOnes - 1) % 2 == 1) {
                acc = negate(acc);
            }
        }
    }
    return (T)acc;
}
//=============================================================================
// Lane mode of the saturating sum: contiguous lines of 44+ use 4 interleaved accumulators,
// strided lines of 2048+ use blocks of 1024 added in order, shorter lines keep index order.
constexpr indexType laneMinimumLength = 44;
constexpr indexType blockMinimumLength = 2048;
constexpr indexType blockLength = 1024;
//=============================================================================
template <class T, class Acc, class StepFn>
static void
nativeSumLanes(const T* sp, T* dp, indexType planes, indexType planesize, indexType linesize,
    StepFn step)
{
    const ompIndexType lineCount = (ompIndexType)(planes * planesize);
    OMP_PARALLEL_FOR_LOOP(lineCount > 1 ? lineCount * (ompIndexType)linesize : 0)
    for (ompIndexType line = 0; line < lineCount; line++) {
        const indexType p = (indexType)line / planesize;
        const indexType j = (indexType)line % planesize;
        const T* first = sp + p * planesize * linesize + j;
        Acc r = 0;
        if (planesize == 1 && linesize >= laneMinimumLength) {
            Acc lanes[4] = { 0, 0, 0, 0 };
            for (indexType k = 0; k < linesize; k++) {
                lanes[k & 3] = step(lanes[k & 3], first[k]);
            }
            r = lanes[0];
            for (int l = 1; l < 4; l++) {
                r = step(r, (T)lanes[l]);
            }
        } else if (planesize > 1 && linesize >= blockMinimumLength) {
            for (indexType b = 0; b < linesize; b += blockLength) {
                const indexType e = std::min(b + blockLength, linesize);
                Acc s = 0;
                for (indexType k = b; k < e; k++) {
                    s = step(s, first[k * planesize]);
                }
                r = step(r, (T)s);
            }
        } else {
            for (indexType k = 0; k < linesize; k++) {
                r = step(r, first[k * planesize]);
            }
        }
        dp[line] = (T)r;
    }
}
//=============================================================================
// Integer sum/prod kept in the integer class: saturates after each step, in index order.
template <class T, bool isProd>
static void
NativeIntegerReduceT(const T* sp, T* dp, indexType planes, indexType planesize, indexType linesize)
{
    // 8 to 32 bit types use a 64 bit accumulator clamped at each step (no overflow possible).
    constexpr bool wide = sizeof(T) < 8;
    using Wide =
        typename std::conditional<isProd && std::is_same<T, uint32>::value, uint64, int64>::type;
    using Acc = typename std::conditional<wide, Wide, T>::type;
    const Acc lo = (Acc)std::numeric_limits<T>::min();
    const Acc hi = (Acc)std::numeric_limits<T>::max();
    const Acc init = isProd ? (Acc)1 : (Acc)0;
    auto step = [lo, hi](Acc a, T v) {
        if constexpr (wide) {
            Acc r = isProd ? a * (Acc)v : a + (Acc)v;
            return std::min(std::max(r, lo), hi);
        } else if constexpr (isProd) {
            return scalar_scalar_integer_times<T>(a, v);
        } else {
            return scalar_scalar_integer_addition<T>(a, v);
        }
    };
    if constexpr (!isProd) {
        if (NelsonConfiguration::getInstance()->getNativeIntegerLanes()) {
            nativeSumLanes<T, Acc>(sp, dp, planes, planesize, linesize, step);
            return;
        }
    }
    if (planes == 1 && planesize == 1 && linesize >= 2 * nativeSumChunk) {
        if constexpr (isProd) {
            dp[0] = nativeProdLongLine<T, Acc>(sp, linesize, lo, hi, step);
        } else {
            dp[0] = nativeSumLongLine<T, Acc>(sp, linesize, lo, hi, step);
        }
        return;
    }
    reduceLinesInOrder(sp, dp, planes, planesize, linesize, init, step);
}
//=============================================================================
// Returns an empty ArrayOf when A is not an integer class (logical included).
template <bool isProd>
static ArrayOf
NativeIntegerReduce(const ArrayOf& A, const Dimensions& dimsRes, indexType planes,
    indexType planesize, indexType linesize)
{
    NelsonType classA = A.dataClass();
    if (!A.isIntegerType()) {
        return { };
    }
    void* ptr = ArrayOf::allocateArrayOf(classA, dimsRes.elementCount(), stringVector(), false);
    const void* sp = A.dataPointer();
#define NLS_NATIVE_INT_CASE(NLSTYPE, CTYPE)                                                        \
    case NLSTYPE:                                                                                  \
        NativeIntegerReduceT<CTYPE, isProd>(                                                       \
            (const CTYPE*)sp, (CTYPE*)ptr, planes, planesize, linesize);                           \
        break;
    switch (classA) {
        NLS_NATIVE_INT_CASE(NLS_INT8, int8)
        NLS_NATIVE_INT_CASE(NLS_UINT8, uint8)
        NLS_NATIVE_INT_CASE(NLS_INT16, int16)
        NLS_NATIVE_INT_CASE(NLS_UINT16, uint16)
        NLS_NATIVE_INT_CASE(NLS_INT32, int32)
        NLS_NATIVE_INT_CASE(NLS_UINT32, uint32)
        NLS_NATIVE_INT_CASE(NLS_INT64, int64)
        NLS_NATIVE_INT_CASE(NLS_UINT64, uint64)
    default:
        break;
    }
#undef NLS_NATIVE_INT_CASE
    return ArrayOf(classA, dimsRes, ptr);
}
//=============================================================================
// Integer (or logical) sum/prod read from the native buffer into a double result, no copy.
template <class T, bool isProd>
static void
IntegerReduceToDoubleT(const T* sp, double* dp, indexType elementCount)
{
    double accum = isProd ? 1.0 : 0.0;
    if constexpr (isProd) {
#if WITH_OPENMP
#pragma omp parallel for reduction(* : accum)
#endif
        for (ompIndexType k = 0; k < (ompIndexType)elementCount; k++) {
            accum *= (double)sp[k];
        }
    } else {
#if WITH_OPENMP
#pragma omp parallel for reduction(+ : accum)
#endif
        for (ompIndexType k = 0; k < (ompIndexType)elementCount; k++) {
            accum += (double)sp[k];
        }
    }
    dp[0] = accum;
}
//=============================================================================
template <class T, bool isProd>
static void
IntegerReduceToDoubleT(
    const T* sp, double* dp, indexType planes, indexType planesize, indexType linesize)
{
    reduceLinesInOrder(sp, dp, planes, planesize, linesize, isProd ? 1.0 : 0.0,
        [](double a, T v) { return isProd ? a * (double)v : a + (double)v; });
}
//=============================================================================
template <bool isProd>
static void
IntegerReduceToDouble(const ArrayOf& A, double* ptr, bool isVector, indexType elementCount,
    indexType planecount, indexType planesize, indexType linesize)
{
    const void* p = A.dataPointer();
    // a single line uses the parallel reduction
    isVector = isVector || (planecount == 1 && planesize == 1);
#define NLS_INT_TO_DOUBLE_CASE(NLSTYPE, CTYPE)                                                     \
    case NLSTYPE:                                                                                  \
        if (isVector) {                                                                            \
            IntegerReduceToDoubleT<CTYPE, isProd>((const CTYPE*)p, ptr, elementCount);             \
        } else {                                                                                   \
            IntegerReduceToDoubleT<CTYPE, isProd>(                                                 \
                (const CTYPE*)p, ptr, planecount, planesize, linesize);                            \
        }                                                                                          \
        break;
    switch (A.dataClass()) {
        NLS_INT_TO_DOUBLE_CASE(NLS_LOGICAL, logical)
        NLS_INT_TO_DOUBLE_CASE(NLS_UINT8, uint8)
        NLS_INT_TO_DOUBLE_CASE(NLS_INT8, int8)
        NLS_INT_TO_DOUBLE_CASE(NLS_UINT16, uint16)
        NLS_INT_TO_DOUBLE_CASE(NLS_INT16, int16)
        NLS_INT_TO_DOUBLE_CASE(NLS_UINT32, uint32)
        NLS_INT_TO_DOUBLE_CASE(NLS_INT32, int32)
        NLS_INT_TO_DOUBLE_CASE(NLS_UINT64, uint64)
        NLS_INT_TO_DOUBLE_CASE(NLS_INT64, int64)
    default:
        break;
    }
#undef NLS_INT_TO_DOUBLE_CASE
}
//=============================================================================
} // namespace Nelson
//=============================================================================
