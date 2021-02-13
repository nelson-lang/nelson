//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _SCL_SECURE_NO_WARNINGS
#endif
#include "lapack_eigen.hpp"
#include <Eigen/Dense>
#include <Eigen/Sparse>
#include "DeterminantMatrix.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
T
DeterminantRealMatrix(T* values, indexType rows, indexType columns)
{
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matA(values, rows, columns);
    return matA.hasNaN() ? (T)std::nan("NaN") : (T)matA.determinant();
}
//=============================================================================
template <class Tz, class Tr>
Tz
DeterminantComplexMatrix(Tr* values, indexType rows, indexType columns)
{
    Tz* pzMat = reinterpret_cast<Tz*>(values);
    Eigen::Map<Eigen::Matrix<Tz, Eigen::Dynamic, Eigen::Dynamic>> matA(pzMat, rows, columns);
    return matA.hasNaN() ? Tz((Tr)std::nan("NaN"), (Tr)std::nan("NaN")) : matA.determinant();
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
    return asDense.hasNaN() ? doublecomplex(std::nan("NaN"), std::nan("NaN"))
                            : asDense.determinant();
}
//=============================================================================
ArrayOf
DeterminantMatrix(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    bool isSupportedTypes = A.isDoubleClass() || (A.isSingleClass() && !A.isSparse());
    if (!isSupportedTypes) {
        needToOverload = true;
        return ArrayOf();
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
                    (double*)A.getDataPointer(), A.getRows(), A.getColumns());
                R = ArrayOf::dcomplexConstructor(result.real(), result.imag());
            } else {
                singlecomplex result = DeterminantComplexMatrix<singlecomplex, single>(
                    (single*)A.getDataPointer(), A.getRows(), A.getColumns());
                R = ArrayOf::dcomplexConstructor(result.real(), result.imag());
            }
        } else {
            if (A.isDoubleClass()) {
                R = ArrayOf::doubleConstructor(DeterminantRealMatrix<double>(
                    (double*)A.getDataPointer(), A.getRows(), A.getColumns()));
            } else {
                R = ArrayOf::singleConstructor(DeterminantRealMatrix<single>(
                    (single*)A.getDataPointer(), A.getRows(), A.getColumns()));
            }
        }
    }
    return R;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
