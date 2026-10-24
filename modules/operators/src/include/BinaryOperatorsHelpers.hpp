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
#include <array>
#include <complex>
#include <string>
#include "ArrayOf.hpp"
#include "MatrixCheck.hpp"
#include "nlsOperators_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
struct BinaryBroadcast
{
    Dimensions dimensions;
    std::array<indexType, MAXDIMS> outputLengths {};
    std::array<indexType, MAXDIMS> aStrides {};
    std::array<indexType, MAXDIMS> bStrides {};
    indexType nDims { 0 };
};
//=============================================================================
NLSOPERATORS_IMPEXP ArrayOf
binaryOperatorEmptyMatrixEmptyMatrix(
    const ArrayOf& A, const ArrayOf& B, NelsonType commonClass, const std::string& operatorName);
//=============================================================================
NLSOPERATORS_IMPEXP BinaryBroadcast
binaryOperatorBroadcast(const ArrayOf& A, const ArrayOf& B, const std::string& operatorName);
//=============================================================================
inline indexType
binaryOperatorBroadcastIndex(const BinaryBroadcast& info, indexType linearIndex,
    const std::array<indexType, MAXDIMS>& strides)
{
    indexType index = 0;
    indexType remaining = linearIndex;
    for (indexType k = 0; k < info.nDims; ++k) {
        indexType coord = remaining % info.outputLengths[k];
        remaining /= info.outputLengths[k];
        index += coord * strides[k];
    }
    return index;
}
//=============================================================================
template <class T, class Operation>
ArrayOf
binaryOperatorBroadcastNumeric(const ArrayOf& A, const ArrayOf& B, NelsonType destinationClass,
    const std::string& operatorName, Operation operation)
{
    BinaryBroadcast info = binaryOperatorBroadcast(A, B, operatorName);
    indexType elementCount = info.dimensions.elementCount();
    T* ptrC = static_cast<T*>(ArrayOf::allocateArrayOf(destinationClass, elementCount));
    const T* ptrA = static_cast<const T*>(A.dataPointer());
    const T* ptrB = static_cast<const T*>(B.dataPointer());
    for (indexType k = 0; k < elementCount; ++k) {
        indexType indexA = binaryOperatorBroadcastIndex(info, k, info.aStrides);
        indexType indexB = binaryOperatorBroadcastIndex(info, k, info.bStrides);
        ptrC[k] = operation(ptrA[indexA], ptrB[indexB]);
    }
    return ArrayOf(destinationClass, info.dimensions, ptrC, false);
}
//=============================================================================
template <class T, class Operation>
ArrayOf
binaryOperatorBroadcastComplex(const ArrayOf& A, const ArrayOf& B, NelsonType destinationClass,
    const std::string& operatorName, Operation operation)
{
    BinaryBroadcast info = binaryOperatorBroadcast(A, B, operatorName);
    indexType elementCount = info.dimensions.elementCount();
    T* ptrC = static_cast<T*>(ArrayOf::allocateArrayOf(destinationClass, elementCount));
    const auto* ptrA = reinterpret_cast<const std::complex<T>*>(A.dataPointer());
    const auto* ptrB = reinterpret_cast<const std::complex<T>*>(B.dataPointer());
    auto* ptrZC = reinterpret_cast<std::complex<T>*>(ptrC);
    for (indexType k = 0; k < elementCount; ++k) {
        indexType indexA = binaryOperatorBroadcastIndex(info, k, info.aStrides);
        indexType indexB = binaryOperatorBroadcastIndex(info, k, info.bStrides);
        ptrZC[k] = operation(ptrA[indexA], ptrB[indexB]);
    }
    return ArrayOf(destinationClass, info.dimensions, ptrC, false);
}
//=============================================================================
using BinaryOperatorKernel = ArrayOf (*)(NelsonType, const ArrayOf&, const ArrayOf&);
//=============================================================================
template <class Fallback>
ArrayOf
binaryOperator2DDispatch(NelsonType destinationClass, const ArrayOf& A, const ArrayOf& B,
    BinaryOperatorKernel scalarScalar, BinaryOperatorKernel scalarMatrix,
    BinaryOperatorKernel matrixScalar, BinaryOperatorKernel matrixMatrix,
    BinaryOperatorKernel rowColumn, BinaryOperatorKernel columnRow, BinaryOperatorKernel rowMatrix,
    BinaryOperatorKernel matrixRow, BinaryOperatorKernel columnMatrix,
    BinaryOperatorKernel matrixColumn, Fallback fallback)
{
    Dimensions dimsA = A.dimensions();
    Dimensions dimsB = B.dimensions();
    if (SameSizeCheck(dimsA, dimsB)) {
        if (A.isScalar()) {
            return scalarScalar(destinationClass, A, B);
        }
        return matrixMatrix(destinationClass, A, B);
    }
    if (A.isScalar() || B.isScalar()) {
        if (A.isScalar()) {
            return scalarMatrix(destinationClass, A, B);
        }
        return matrixScalar(destinationClass, A, B);
    }
    if (A.isVector() || B.isVector()) {
        if ((A.isRowVector() && B.isRowVector()) || (A.isColumnVector() && B.isColumnVector())) {
            return fallback(A, B, destinationClass);
        }
        if (A.isRowVector() && B.isColumnVector()) {
            return rowColumn(destinationClass, A, B);
        }
        if (A.isColumnVector() && B.isRowVector()) {
            return columnRow(destinationClass, A, B);
        }
        if (A.rows() == B.rows()) {
            if (A.isVector()) {
                if (!B.is2D()) {
                    return fallback(A, B, destinationClass);
                }
                return rowMatrix(destinationClass, A, B);
            }
            if (!A.is2D()) {
                return fallback(A, B, destinationClass);
            }
            return matrixRow(destinationClass, A, B);
        }
        if (A.columns() == B.columns()) {
            if (A.isVector()) {
                if (!B.is2D()) {
                    return fallback(A, B, destinationClass);
                }
                return columnMatrix(destinationClass, A, B);
            }
            if (!A.is2D()) {
                return fallback(A, B, destinationClass);
            }
            return matrixColumn(destinationClass, A, B);
        }
        return fallback(A, B, destinationClass);
    }
    return fallback(A, B, destinationClass);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
