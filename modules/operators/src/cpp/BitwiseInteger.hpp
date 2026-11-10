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
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "ArrayOf.hpp"
#include "BitwiseOperators.hpp"
#include "BinaryOperatorsHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Expands two 2-D operands (rows or columns of length one) without index arithmetic per element.
template <class S, class O, class F>
bool
bitwiseBroadcast2D(const ArrayOf& A, const ArrayOf& B, NelsonType outClass, F op, ArrayOf& res)
{
    if (!A.is2D() || !B.is2D()) {
        return false;
    }
    indexType aR = A.rows(), aC = A.columns(), bR = B.rows(), bC = B.columns();
    indexType M = (aR == 1) ? bR : aR;
    indexType N = (aC == 1) ? bC : aC;
    indexType len = M * N;
    O* out = static_cast<O*>(ArrayOf::allocateArrayOf(outClass, len));
    res = ArrayOf(outClass, Dimensions(M, N), out, false);
    const S* pa = static_cast<const S*>(A.dataPointer());
    const S* pb = static_cast<const S*>(B.dataPointer());
    bool aRows = aR != 1;
    bool bRows = bR != 1;
    indexType aColStride = (aC == 1) ? 0 : aR;
    indexType bColStride = (bC == 1) ? 0 : bR;
    OMP_PARALLEL_FOR_LOOP(len)
    for (ompIndexType j = 0; j < (ompIndexType)N; ++j) {
        const S* ca = pa + j * aColStride;
        const S* cb = pb + j * bColStride;
        O* co = out + j * M;
        if (aRows && bRows) {
            for (indexType i = 0; i < M; ++i) {
                co[i] = op(ca[i], cb[i]);
            }
        } else if (aRows) {
            const S b0 = cb[0];
            for (indexType i = 0; i < M; ++i) {
                co[i] = op(ca[i], b0);
            }
        } else if (bRows) {
            const S a0 = ca[0];
            for (indexType i = 0; i < M; ++i) {
                co[i] = op(a0, cb[i]);
            }
        } else {
            const O v = op(ca[0], cb[0]);
            for (indexType i = 0; i < M; ++i) {
                co[i] = v;
            }
        }
    }
    return true;
}
//=============================================================================
// Applies op element-wise with scalar, same-size and implicit expansion cases; op is inlined.
template <class S, class O, class F>
ArrayOf
bitwiseApply(const ArrayOf& A, const ArrayOf& B, NelsonType outClass, F op)
{
    bool sameSize = A.dimensions().equals(B.dimensions());
    if (!sameSize && !A.isScalar() && !B.isScalar()) {
        ArrayOf res;
        if (bitwiseBroadcast2D<S, O>(A, B, outClass, op, res)) {
            return res;
        }
        return binaryOperatorBroadcastNumeric<O>(A, B, outClass, "bitwise", op);
    }
    Dimensions dims = (A.isScalar() && !sameSize) ? B.dimensions() : A.dimensions();
    indexType len = dims.elementCount();
    O* out = static_cast<O*>(ArrayOf::allocateArrayOf(outClass, len));
    ArrayOf res(outClass, dims, out, false);
    const S* pa = static_cast<const S*>(A.dataPointer());
    const S* pb = static_cast<const S*>(B.dataPointer());
    if (!sameSize && A.isScalar()) {
        const S a = pa[0];
        OMP_PARALLEL_FOR_LOOP(len)
        for (ompIndexType k = 0; k < (ompIndexType)len; ++k) {
            out[k] = op(a, pb[k]);
        }
    } else if (!sameSize && B.isScalar()) {
        const S b = pb[0];
        OMP_PARALLEL_FOR_LOOP(len)
        for (ompIndexType k = 0; k < (ompIndexType)len; ++k) {
            out[k] = op(pa[k], b);
        }
    } else {
        OMP_PARALLEL_FOR_LOOP(len)
        for (ompIndexType k = 0; k < (ompIndexType)len; ++k) {
            out[k] = op(pa[k], pb[k]);
        }
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
integer_bitwise(BITWISE_OPERATOR bitwiseOperator, NelsonType classDestination, const ArrayOf& A,
    const ArrayOf& B)
{
    switch (bitwiseOperator) {
    case BITWISE_OPERATOR::BIT_AND:
        return bitwiseApply<T, T>(A, B, classDestination, [](T a, T b) { return T(a & b); });
    case BITWISE_OPERATOR::BIT_OR:
        return bitwiseApply<T, T>(A, B, classDestination, [](T a, T b) { return T(a | b); });
    case BITWISE_OPERATOR::BIT_XOR:
    default:
        return bitwiseApply<T, T>(A, B, classDestination, [](T a, T b) { return T(a ^ b); });
    }
}
//=============================================================================
// Double values worked in the integer class T, result back to double.
template <class T>
ArrayOf
double_bitwise(BITWISE_OPERATOR bitwiseOperator, const ArrayOf& A, const ArrayOf& B)
{
    switch (bitwiseOperator) {
    case BITWISE_OPERATOR::BIT_AND:
        return bitwiseApply<double, double>(A, B, NLS_DOUBLE,
            [](double a, double b) { return static_cast<double>(T(T(a) & T(b))); });
    case BITWISE_OPERATOR::BIT_OR:
        return bitwiseApply<double, double>(A, B, NLS_DOUBLE,
            [](double a, double b) { return static_cast<double>(T(T(a) | T(b))); });
    case BITWISE_OPERATOR::BIT_XOR:
    default:
        return bitwiseApply<double, double>(A, B, NLS_DOUBLE,
            [](double a, double b) { return static_cast<double>(T(T(a) ^ T(b))); });
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
