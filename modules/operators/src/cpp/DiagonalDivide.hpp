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
#include "lapack_eigen_config.hpp"
#include "LapackPrototypes.hpp"
#include <Eigen/Core>
#include <algorithm>
#include <cmath>
#include <cstdint>
#include <cstring>
#include <limits>
#include <type_traits>
#include "ArrayOf.hpp"
#include "LuFactorization.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// True when the n x n column-major matrix has only zeros off its diagonal.
template <class T>
inline bool
hasZeroOffDiagonal(const T* a, indexType n)
{
    // complex entries are scanned as pairs of real parts
    // bit patterns OR-ed, sign bit dropped per column: -0 counts as zero, NaN does not
    using Real = typename Eigen::NumTraits<T>::Real;
    using Bits = std::conditional_t<sizeof(Real) == 8, uint64_t, uint32_t>;
    constexpr indexType parts = sizeof(T) / sizeof(Real);
    const auto* bytes = reinterpret_cast<const unsigned char*>(a);
    const indexType columnLength = n * parts;
    for (indexType col = 0; col < n; ++col) {
        const unsigned char* column = bytes + col * columnLength * sizeof(Bits);
        const indexType diagonalStart = col * parts;
        Bits nonZero = 0;
        for (indexType k = 0; k < diagonalStart; ++k) {
            Bits value;
            std::memcpy(&value, column + k * sizeof(Bits), sizeof(Bits));
            nonZero |= value;
        }
        for (indexType k = diagonalStart + parts; k < columnLength; ++k) {
            Bits value;
            std::memcpy(&value, column + k * sizeof(Bits), sizeof(Bits));
            nonZero |= value;
        }
        if ((nonZero << 1) != 0) {
            return false;
        }
    }
    return true;
}
//=============================================================================
// True when the smallest over largest diagonal entry is at least eps for the triangular solver.
template <class T>
inline bool
hasDiagonalRatioOfAtLeastEps(const T* a, indexType n)
{
    using Real = typename Eigen::NumTraits<T>::Real;
    constexpr Real eps = std::numeric_limits<Real>::epsilon();
    if constexpr (Eigen::NumTraits<T>::IsComplex) {
        // complex sizes are measured in several ways, all within 2x of max(|re|, |im|)
        Real minBound = std::numeric_limits<Real>::infinity();
        Real maxBound = 0;
        for (indexType k = 0; k < n; ++k) {
            const T& d = a[k + k * n];
            Real value = std::max(std::abs(d.real()), std::abs(d.imag()));
            minBound = std::min(minBound, value);
            maxBound = std::max(maxBound, value);
        }
        return minBound / maxBound >= 2 * eps;
    } else {
        Real minDiagonal = std::numeric_limits<Real>::infinity();
        Real maxDiagonal = 0;
        for (indexType k = 0; k < n; ++k) {
            Real value = std::abs(a[k + k * n]);
            minDiagonal = std::min(minDiagonal, value);
            maxDiagonal = std::max(maxDiagonal, value);
        }
        return minDiagonal / maxDiagonal >= eps;
    }
}
//=============================================================================
// Solves D \ B (OnRight false, B is n x cols) or B / D (OnRight true, B is rows x n) for a
// diagonal D with a diagonal ratio of at least eps and finite quotients; else nullptr.
// Other cases go to the triangular solver, which gives the warnings and Inf/NaN spreading.
template <class T, bool OnRight>
inline T*
solveWellConditionedDiagonal(
    NelsonType dataClass, const T* a, indexType n, const T* b, indexType rows, indexType cols)
{
    using Real = typename Eigen::NumTraits<T>::Real;
    if (!hasZeroOffDiagonal<T>(a, n)) {
        return nullptr;
    }
    if (!hasDiagonalRatioOfAtLeastEps<T>(a, n)) {
        return nullptr;
    }
    T* x = static_cast<T*>(ArrayOf::allocateArrayOf(dataClass, rows * cols));
    const Real* parts = reinterpret_cast<const Real*>(x);
    constexpr indexType partCount = sizeof(T) / sizeof(Real);
    Real finiteCheck = 0;
    for (indexType col = 0; col < cols; ++col) {
        const T* bc = b + col * rows;
        T* xc = x + col * rows;
        if constexpr (OnRight) {
            const T d = a[col + col * n];
            for (indexType k = 0; k < rows; ++k) {
                xc[k] = luDivide(bc[k], d);
            }
        } else {
            for (indexType k = 0; k < rows; ++k) {
                xc[k] = luDivide(bc[k], a[k + k * n]);
            }
        }
        const Real* xp = parts + col * rows * partCount;
        for (indexType k = 0; k < rows * partCount; ++k) {
            finiteCheck += xp[k] - xp[k];
        }
    }
    if (finiteCheck != Real(0)) {
        // the buffer was allocated as an array of real parts
        delete[] reinterpret_cast<Real*>(x);
        return nullptr;
    }
    return x;
}
//=============================================================================
// D \ B (OnRight false) or B / D (OnRight true) for dense float operands of one class.
// solved is false when D is not a well conditioned diagonal.
template <bool OnRight>
inline ArrayOf
denseDiagonalDivide(const ArrayOf& D, const ArrayOf& B, bool& solved)
{
    solved = false;
    const indexType n = D.rows();
    if (n != D.columns()) {
        return {};
    }
    const NelsonType dataClass = D.dataClass();
    const indexType rows = B.rows();
    const indexType cols = B.columns();
    const void* a = D.dataPointer();
    const void* b = B.dataPointer();
    void* x = nullptr;
    switch (dataClass) {
    case NLS_DOUBLE:
        x = solveWellConditionedDiagonal<double, OnRight>(
            dataClass, static_cast<const double*>(a), n, static_cast<const double*>(b), rows, cols);
        break;
    case NLS_SINGLE:
        x = solveWellConditionedDiagonal<single, OnRight>(
            dataClass, static_cast<const single*>(a), n, static_cast<const single*>(b), rows, cols);
        break;
    case NLS_DCOMPLEX:
        x = solveWellConditionedDiagonal<doublecomplex, OnRight>(dataClass,
            static_cast<const doublecomplex*>(a), n, static_cast<const doublecomplex*>(b), rows,
            cols);
        break;
    case NLS_SCOMPLEX:
        x = solveWellConditionedDiagonal<singlecomplex, OnRight>(dataClass,
            static_cast<const singlecomplex*>(a), n, static_cast<const singlecomplex*>(b), rows,
            cols);
        break;
    default:
        break;
    }
    if (x == nullptr) {
        return {};
    }
    solved = true;
    return ArrayOf(dataClass, Dimensions(rows, cols), x, false);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
