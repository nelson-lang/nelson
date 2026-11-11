//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "lapack_eigen_config.hpp"
#include <Eigen/Sparse>
#include <algorithm>
#include <cmath>
#include <complex>
#include <vector>
#include "IsApproxNorm.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    // 0 finite, 1 +Inf, 2 -Inf, 3 NaN
    inline int
    specialKind(double v)
    {
        if (std::isnan(v)) {
            return 3;
        }
        if (std::isinf(v)) {
            return v > 0 ? 1 : 2;
        }
        return 0;
    }
    //=============================================================================
    inline bool
    normRule(double diff, double normA, double normB, double precision)
    {
        return diff <= precision * precision * std::min(normA, normB);
    }
    //=============================================================================
    // sums of squares without branches, four lanes so the loop can be vectorized
    // decided is false when special values or an overflow need the exact path
    bool
    fastIsApprox(const double* pa, const double* pb, indexType n, indexType stride,
        double precision, bool& decided)
    {
        double d[4] = { 0., 0., 0., 0. };
        double sa[4] = { 0., 0., 0., 0. };
        double sb[4] = { 0., 0., 0., 0. };
        indexType k = 0;
        for (; k + 4 <= n; k += 4) {
            for (int l = 0; l < 4; ++l) {
                const double a = pa[(k + l) * stride];
                const double b = pb[(k + l) * stride];
                d[l] += (a - b) * (a - b);
                sa[l] += a * a;
                sb[l] += b * b;
            }
        }
        for (; k < n; ++k) {
            const double a = pa[k * stride];
            const double b = pb[k * stride];
            d[0] += (a - b) * (a - b);
            sa[0] += a * a;
            sb[0] += b * b;
        }
        const double diff = (d[0] + d[1]) + (d[2] + d[3]);
        const double normA = (sa[0] + sa[1]) + (sa[2] + sa[3]);
        const double normB = (sb[0] + sb[1]) + (sb[2] + sb[3]);
        decided = std::isfinite(diff) && std::isfinite(normA) && std::isfinite(normB);
        return decided && normRule(diff, normA, normB, precision);
    }
    //=============================================================================
    // special values must sit at the same places and are then ignored
    // magnitudes above 1 are scaled down to avoid overflow, the rule does not change with scale
    bool
    exactIsApprox(
        const double* pa, const double* pb, indexType n, indexType stride, double precision)
    {
        double maxAbs = 0.;
        for (indexType k = 0; k < n; ++k) {
            const double a = pa[k * stride];
            const double b = pb[k * stride];
            const int kind = specialKind(a);
            if (kind != specialKind(b)) {
                return false;
            }
            if (kind == 0) {
                maxAbs = std::max(maxAbs, std::max(std::fabs(a), std::fabs(b)));
            }
        }
        const double scale = std::max(1., maxAbs);
        double diff = 0.;
        double normA = 0.;
        double normB = 0.;
        for (indexType k = 0; k < n; ++k) {
            const double a = pa[k * stride];
            if (specialKind(a) != 0) {
                continue;
            }
            const double sa = a / scale;
            const double sb = pb[k * stride] / scale;
            diff += (sa - sb) * (sa - sb);
            normA += sa * sa;
            normB += sb * sb;
        }
        return normRule(diff, normA, normB, precision);
    }
    //=============================================================================
    bool
    partIsApprox(
        const double* pa, const double* pb, indexType n, indexType stride, double precision)
    {
        bool decided = false;
        bool result = fastIsApprox(pa, pb, n, stride, precision, decided);
        return decided ? result : exactIsApprox(pa, pb, n, stride, precision);
    }
    //=============================================================================
    // real and imaginary parts are checked separately, interleaved complex storage
    bool
    valuesIsApprox(
        const double* pa, const double* pb, indexType n, bool asComplex, double precision)
    {
        if (!asComplex) {
            return partIsApprox(pa, pb, n, 1, precision);
        }
        return partIsApprox(pa, pb, n, 2, precision)
            && partIsApprox(pa + 1, pb + 1, n, 2, precision);
    }
    //=============================================================================
    template <class T> using SparseOf = Eigen::SparseMatrix<T, 0, signedIndexType>;
    //=============================================================================
    // stored nonzero values must share one pattern, zeros kept by the storage are skipped
    template <class T>
    bool
    sparseIsApprox(const ArrayOf& A, const ArrayOf& B, bool asComplex, double precision)
    {
        const auto* spA = static_cast<const SparseOf<T>*>(A.sparseDataPointer());
        const auto* spB = static_cast<const SparseOf<T>*>(B.sparseDataPointer());
        std::vector<T> valuesA;
        std::vector<T> valuesB;
        valuesA.reserve(spA->nonZeros());
        valuesB.reserve(spB->nonZeros());
        for (signedIndexType col = 0; col < spA->outerSize(); ++col) {
            typename SparseOf<T>::InnerIterator itA(*spA, col);
            typename SparseOf<T>::InnerIterator itB(*spB, col);
            while (true) {
                while (itA && itA.value() == T(0)) {
                    ++itA;
                }
                while (itB && itB.value() == T(0)) {
                    ++itB;
                }
                if (!itA || !itB) {
                    if (itA || itB) {
                        return false;
                    }
                    break;
                }
                if (itA.row() != itB.row()) {
                    return false;
                }
                valuesA.push_back(itA.value());
                valuesB.push_back(itB.value());
                ++itA;
                ++itB;
            }
        }
        return valuesIsApprox(reinterpret_cast<const double*>(valuesA.data()),
            reinterpret_cast<const double*>(valuesB.data()), static_cast<indexType>(valuesA.size()),
            asComplex, precision);
    }
    //=============================================================================
}
//=============================================================================
bool
IsApproxNorm(const ArrayOf& A, const ArrayOf& B, double precision)
{
    // dense real double 2-D inputs are read in place, without copy or conversion
    indexType countA = 0, rowsA = 0, columnsA = 0;
    indexType countB = 0, rowsB = 0, columnsB = 0;
    const double* dataA = A.realDoubleData(countA, rowsA, columnsA);
    const double* dataB = dataA ? B.realDoubleData(countB, rowsB, columnsB) : nullptr;
    if (dataB != nullptr && A.nDims() <= 2 && B.nDims() <= 2) {
        if (rowsA != rowsB || columnsA != columnsB) {
            return false;
        }
        return countA == 0 || valuesIsApprox(dataA, dataB, countA, false, precision);
    }
    Dimensions dimsA = A.dimensions();
    Dimensions dimsB = B.dimensions();
    if (A.isEmpty() || B.isEmpty()) {
        return dimsA.equals(dimsB);
    }
    if (!dimsA.equals(dimsB)) {
        return false;
    }
    bool asComplex = A.isComplex() || B.isComplex();
    ArrayOf a(A);
    ArrayOf b(B);
    a.promoteType(asComplex ? NLS_DCOMPLEX : NLS_DOUBLE);
    b.promoteType(asComplex ? NLS_DCOMPLEX : NLS_DOUBLE);
    if (a.isSparse() || b.isSparse()) {
        a.makeSparse();
        b.makeSparse();
        return asComplex ? sparseIsApprox<std::complex<double>>(a, b, true, precision)
                         : sparseIsApprox<double>(a, b, false, precision);
    }
    return valuesIsApprox(static_cast<const double*>(a.dataPointer()),
        static_cast<const double*>(b.dataPointer()), dimsA.elementCount(), asComplex, precision);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
