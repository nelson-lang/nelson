//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _SCL_SECURE_NO_WARNINGS
#endif
#include "nlsBuildConfig.h"
#include "lapack_eigen_config.hpp"
#include "LapackPrototypes.hpp"
#include "LuFactorization.hpp"
#include <Eigen/Dense>
#include <Eigen/Sparse>
#include <algorithm>
#include <vector>
#include "DeterminantMatrix.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Dense determinant from the same LU as the linear solvers.
template <class T>
static T
denseLuDeterminant(const T* values, indexType n)
{
    lapack_int N = (lapack_int)n;
    lapack_int info = 0;
    // small matrices are factored on the stack
    if (isSmallLu<T>(N, N)) {
        T factor[16 * 16];
        lapack_int ipiv[16];
        std::copy(values, values + (size_t)n * (size_t)n, factor);
        smallLuFactor<T>(N, N, factor, N, ipiv, &info);
        return luDeterminant(factor, N, ipiv);
    }
    std::vector<T> factor(values, values + (size_t)n * (size_t)n);
    std::vector<lapack_int> ipiv((size_t)n);
    luFactor(&N, &N, factor.data(), &N, ipiv.data(), &info);
    return luDeterminant(factor.data(), N, ipiv.data());
}
//=============================================================================
template <class T>
T
DeterminantRealMatrix(T* values, indexType rows, indexType columns)
{
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matA(values, rows, columns);
    return matA.hasNaN() ? (T)std::nan("") : denseLuDeterminant<T>(values, rows);
}
//=============================================================================
template <class Tz, class Tr>
Tz
DeterminantComplexMatrix(Tr* values, indexType rows, indexType columns)
{
    Tz* pzMat = reinterpret_cast<Tz*>(values);
    Eigen::Map<Eigen::Matrix<Tz, Eigen::Dynamic, Eigen::Dynamic>> matA(pzMat, rows, columns);
    return matA.hasNaN() ? Tz((Tr)std::nan(""), (Tr)std::nan(""))
                         : denseLuDeterminant<Tz>(pzMat, rows);
}
//=============================================================================
template <class T>
static T
DeterminantSparseRealMatrix(const void* ptrValues)
{
    auto* spMatA = (Eigen::SparseMatrix<T, 0, signedIndexType>*)ptrValues;
    return spMatA->toDense().determinant();
}
//=============================================================================
template <class Tz, class Tr>
static Tz
DeterminantSparseComplexMatrix(const void* ptrValues)
{
    auto* spMatA = (Eigen::SparseMatrix<Tz, 0, signedIndexType>*)ptrValues;
    Eigen::Matrix<Tz, Eigen::Dynamic, Eigen::Dynamic> asDense = spMatA->toDense();
    return asDense.hasNaN() ? Tz((Tr)std::nan(""), (Tr)std::nan("")) : asDense.determinant();
}
//=============================================================================
ArrayOf
DeterminantMatrix(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    bool isSupportedTypes = A.isDoubleClass() || A.isSingleClass();
    if (!isSupportedTypes) {
        needToOverload = true;
        return {};
    }
    if (!A.isSquare()) {
        Error("Nelson:linear_algebra:squareMatrixExpected", "%s expected.", "Square matrix");
    }
    if (A.isEmpty()) {
        if (A.isDoubleClass()) {
            return ArrayOf::doubleScalar(1.0);
        }
        return ArrayOf::singleScalar(1.0);
    }
    ArrayOf R;
    Dimensions dimsA = A.dimensions();
    if (A.isSparse()) {
        if (A.isComplex()) {
            if (A.isDoubleClass()) {
                doublecomplex result
                    = DeterminantSparseComplexMatrix<doublecomplex, double>(A.sparseDataPointer());
                R = ArrayOf::dsingleComplexScalar(result.real(), result.imag());
            } else {
                singlecomplex result
                    = DeterminantSparseComplexMatrix<singlecomplex, single>(A.sparseDataPointer());
                R = ArrayOf::singleComplexScalar(result.real(), result.imag());
            }
        } else {
            if (A.isDoubleClass()) {
                R = ArrayOf::doubleScalar(
                    DeterminantSparseRealMatrix<double>(A.sparseDataPointer()));
            } else {
                R = ArrayOf::singleScalar(
                    DeterminantSparseRealMatrix<single>(A.sparseDataPointer()));
            }
        }
    } else {
        if (A.isComplex()) {
            if (A.isDoubleClass()) {
                doublecomplex result = DeterminantComplexMatrix<doublecomplex, double>(
                    (double*)A.dataPointer(), dimsA.rows(), dimsA.columns());
                R = ArrayOf::dsingleComplexScalar(result.real(), result.imag());
            } else {
                singlecomplex result = DeterminantComplexMatrix<singlecomplex, single>(
                    (single*)A.dataPointer(), dimsA.rows(), dimsA.columns());
                R = ArrayOf::singleComplexScalar(result.real(), result.imag());
            }
        } else {
            if (A.isDoubleClass()) {
                R = ArrayOf::doubleScalar(DeterminantRealMatrix<double>(
                    (double*)A.dataPointer(), dimsA.rows(), dimsA.columns()));
            } else {
                R = ArrayOf::singleScalar(DeterminantRealMatrix<single>(
                    (single*)A.dataPointer(), dimsA.rows(), dimsA.columns()));
            }
        }
    }
    return R;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
