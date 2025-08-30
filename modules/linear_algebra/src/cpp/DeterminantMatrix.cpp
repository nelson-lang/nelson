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
#include <Eigen/Dense>
#include <Eigen/Sparse>
#include "DeterminantMatrix.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
T
DeterminantRealMatrix(T* values, indexType rows, indexType columns)
{
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matA(values, rows, columns);
    return matA.hasNaN() ? (T)std::nan("") : (T)matA.determinant();
}
//=============================================================================
template <class Tz, class Tr>
Tz
DeterminantComplexMatrix(Tr* values, indexType rows, indexType columns)
{
    Tz* pzMat = reinterpret_cast<Tz*>(values);
    Eigen::Map<Eigen::Matrix<Tz, Eigen::Dynamic, Eigen::Dynamic>> matA(pzMat, rows, columns);
    return matA.hasNaN() ? Tz((Tr)std::nan(""), (Tr)std::nan("")) : matA.determinant();
}
//=============================================================================
static double
DeterminantSparseRealMatrix(const void* ptrValues)
{
    Eigen::SparseMatrix<double, 0, signedIndexType>* spMatA
        = (Eigen::SparseMatrix<double, 0, signedIndexType>*)ptrValues;
    return spMatA->toDense().determinant();
}
//=============================================================================
static doublecomplex
DeterminantSparseComplexMatrix(const void* ptrValues)
{
    Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>* spMatA
        = (Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>*)ptrValues;
    Eigen::Matrix<doublecomplex, Eigen::Dynamic, Eigen::Dynamic> asDense = spMatA->toDense();
    return asDense.hasNaN() ? doublecomplex(std::nan(""), std::nan("")) : asDense.determinant();
}
//=============================================================================
ArrayOf
DeterminantMatrix(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    bool isSupportedTypes = A.isDoubleClass() || (A.isSingleClass() && !A.isSparse());
    if (!isSupportedTypes) {
        needToOverload = true;
        return {};
    }
    if (!A.isSquare()) {
        Error(_("Square matrix expected."));
    }
    if (A.isEmpty()) {
        if (A.isDoubleClass()) {
            return ArrayOf::doubleConstructor(1.0);
        }
        return ArrayOf::singleConstructor(1.0);
    }
    ArrayOf R;
    Dimensions dimsA = A.getDimensions();
    if (A.isSparse()) {
        if (A.isComplex()) {
            doublecomplex result = DeterminantSparseComplexMatrix(A.getSparseDataPointer());
            R = ArrayOf::dcomplexConstructor(result.real(), result.imag());
        } else {
            R = ArrayOf::doubleConstructor(DeterminantSparseRealMatrix(A.getSparseDataPointer()));
        }
    } else {
        if (A.isComplex()) {
            if (A.isDoubleClass()) {
                doublecomplex result = DeterminantComplexMatrix<doublecomplex, double>(
                    (double*)A.getDataPointer(), dimsA.getRows(), dimsA.getColumns());
                R = ArrayOf::dcomplexConstructor(result.real(), result.imag());
            } else {
                singlecomplex result = DeterminantComplexMatrix<singlecomplex, single>(
                    (single*)A.getDataPointer(), dimsA.getRows(), dimsA.getColumns());
                R = ArrayOf::dcomplexConstructor(result.real(), result.imag());
            }
        } else {
            if (A.isDoubleClass()) {
                R = ArrayOf::doubleConstructor(DeterminantRealMatrix<double>(
                    (double*)A.getDataPointer(), dimsA.getRows(), dimsA.getColumns()));
            } else {
                R = ArrayOf::singleConstructor(DeterminantRealMatrix<single>(
                    (single*)A.getDataPointer(), dimsA.getRows(), dimsA.getColumns()));
            }
        }
    }
    return R;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
