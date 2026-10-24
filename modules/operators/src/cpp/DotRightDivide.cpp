//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <vector>
#include "DotRightDivide.hpp"
#include "MatrixCheck.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "DotRightDivideReal.hpp"
#include "DotRightDivideComplex.hpp"
#include "DotRightDivideInteger.hpp"
#include "BinaryOperatorsHelpers.hpp"
#include "IntegerCompatibilityHelpers.hpp"
#include "SparseOperatorsHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
static T
runtimeZero()
{
    volatile double zero = 0.;
    double value = zero;
    using RealScalar = typename Eigen::NumTraits<T>::Real;
    return T(static_cast<RealScalar>(value));
}
//=============================================================================
template <class T>
static ArrayOf
sparseDotRightDivideDenseResult(NelsonType dataClass, const ArrayOf& A, const ArrayOf& B,
    const BinaryBroadcast& info, const NelsonSparseMatrix<T>* sparseA,
    const NelsonSparseMatrix<T>* sparseB, const T* denseA, const T* denseB)
{
    T* ptrC = static_cast<T*>(ArrayOf::allocateArrayOf(dataClass, info.dimensions.elementCount()));
    for (indexType idx = 0; idx < info.dimensions.elementCount(); ++idx) {
        indexType indexA = binaryOperatorBroadcastIndex(info, idx, info.aStrides);
        indexType indexB = binaryOperatorBroadcastIndex(info, idx, info.bStrides);
        T numerator;
        if (sparseA) {
            numerator = sparseA->coeff(static_cast<signedIndexType>(indexA % A.rows()),
                static_cast<signedIndexType>(indexA / A.rows()));
        } else {
            numerator = denseA[indexA];
        }
        T denominator;
        if (sparseB) {
            denominator = sparseB->coeff(static_cast<signedIndexType>(indexB % B.rows()),
                static_cast<signedIndexType>(indexB / B.rows()));
        } else {
            denominator = denseB[indexB];
        }
        ptrC[idx] = numerator / denominator;
    }
    return ArrayOf(dataClass, info.dimensions, ptrC, false);
}
//=============================================================================
template <class T>
static ArrayOf
sparseNumeratorDotRightDivideMatrix(NelsonType dataClass, const ArrayOf& A, const ArrayOf& B)
{
    BinaryBroadcast info = binaryOperatorBroadcast(A, B, "./");
    indexType rows = info.dimensions.rows();
    std::vector<Eigen::Triplet<T, signedIndexType>> triplets;
    triplets.reserve(static_cast<size_t>(info.dimensions.elementCount()));
    const auto* sparseA = getSparseMatrixPointer<T>(A);
    const auto* sparseB = getSparseMatrixPointer<T>(B);
    for (indexType idx = 0; idx < info.dimensions.elementCount(); ++idx) {
        indexType indexA = binaryOperatorBroadcastIndex(info, idx, info.aStrides);
        indexType indexB = binaryOperatorBroadcastIndex(info, idx, info.bStrides);
        T numerator = sparseA->coeff(static_cast<signedIndexType>(indexA % A.rows()),
            static_cast<signedIndexType>(indexA / A.rows()));
        T denominator = sparseB->coeff(static_cast<signedIndexType>(indexB % B.rows()),
            static_cast<signedIndexType>(indexB / B.rows()));
        T value = numerator / denominator;
        if (value != T(0)) {
            triplets.emplace_back(static_cast<signedIndexType>(idx % rows),
                static_cast<signedIndexType>(idx / rows), value);
        }
    }
    auto* sparseResult = new NelsonSparseMatrix<T>(rows, info.dimensions.columns());
    sparseResult->setFromTriplets(triplets.begin(), triplets.end());
    return makeSparseOperatorResult<T>(dataClass, info.dimensions, sparseResult);
}
//=============================================================================
template <class T>
static ArrayOf
sparseNumeratorDotRightDivideScalarZero(
    NelsonType dataClass, const ArrayOf& A, const Dimensions& dimensions)
{
    indexType rows = dimensions.rows();
    indexType cols = dimensions.columns();
    std::vector<Eigen::Triplet<T, signedIndexType>> triplets;
    triplets.reserve(static_cast<size_t>(dimensions.elementCount()));
    const auto* sparseA = getSparseMatrixPointer<T>(A);
    T zero = runtimeZero<T>();
    for (indexType col = 0; col < cols; ++col) {
        for (indexType row = 0; row < rows; ++row) {
            signedIndexType sparseRow = static_cast<signedIndexType>(row);
            signedIndexType sparseCol = static_cast<signedIndexType>(col);
            T value = sparseA->coeff(sparseRow, sparseCol) / zero;
            if (value != T(0)) {
                triplets.emplace_back(sparseRow, sparseCol, value);
            }
        }
    }
    auto* sparseResult = new NelsonSparseMatrix<T>(rows, cols);
    sparseResult->setFromTriplets(triplets.begin(), triplets.end());
    return makeSparseOperatorResult<T>(dataClass, dimensions, sparseResult);
}
//=============================================================================
template <class T>
static ArrayOf
sparseNumeratorDenseDenominatorDotRightDivide(
    NelsonType dataClass, const ArrayOf& A, const ArrayOf& B)
{
    BinaryBroadcast info = binaryOperatorBroadcast(A, B, "./");
    const auto* sparseA = getSparseMatrixPointer<T>(A);
    const T* ptrB = static_cast<const T*>(B.dataPointer());
    if (info.dimensions.length() > 2) {
        return sparseDotRightDivideDenseResult<T>(
            dataClass, A, B, info, sparseA, nullptr, nullptr, ptrB);
    }
    indexType rows = info.dimensions.rows();
    std::vector<Eigen::Triplet<T, signedIndexType>> triplets;
    triplets.reserve(static_cast<size_t>(info.dimensions.elementCount()));
    for (indexType idx = 0; idx < info.dimensions.elementCount(); ++idx) {
        indexType indexA = binaryOperatorBroadcastIndex(info, idx, info.aStrides);
        indexType indexB = binaryOperatorBroadcastIndex(info, idx, info.bStrides);
        T numerator = sparseA->coeff(static_cast<signedIndexType>(indexA % A.rows()),
            static_cast<signedIndexType>(indexA / A.rows()));
        T value = numerator / ptrB[indexB];
        if (value != T(0)) {
            triplets.emplace_back(static_cast<signedIndexType>(idx % rows),
                static_cast<signedIndexType>(idx / rows), value);
        }
    }
    auto* sparseResult = new NelsonSparseMatrix<T>(rows, info.dimensions.columns());
    sparseResult->setFromTriplets(triplets.begin(), triplets.end());
    return makeSparseOperatorResult<T>(dataClass, info.dimensions, sparseResult);
}
//=============================================================================
template <class T>
static ArrayOf
denseNumeratorSparseDenominatorDotRightDivide(
    NelsonType dataClass, const ArrayOf& A, const ArrayOf& B)
{
    BinaryBroadcast info = binaryOperatorBroadcast(A, B, "./");
    T* ptrC = static_cast<T*>(ArrayOf::allocateArrayOf(dataClass, info.dimensions.elementCount()));
    const T* ptrA = static_cast<const T*>(A.dataPointer());
    const auto* sparseB = getSparseMatrixPointer<T>(B);
    for (indexType idx = 0; idx < info.dimensions.elementCount(); ++idx) {
        indexType indexA = binaryOperatorBroadcastIndex(info, idx, info.aStrides);
        indexType indexB = binaryOperatorBroadcastIndex(info, idx, info.bStrides);
        T denominator = sparseB->coeff(static_cast<signedIndexType>(indexB % B.rows()),
            static_cast<signedIndexType>(indexB / B.rows()));
        ptrC[idx] = ptrA[indexA] / denominator;
    }
    return ArrayOf(dataClass, info.dimensions, ptrC, false);
}
//=============================================================================
template <class T>
static ArrayOf
sparseDotRightDivide(NelsonType dataClass, const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    needToOverload = false;
    if (!A.isSparse()) {
        needToOverload = true;
        return {};
    }
    if (!B.isSparse()) {
        return sparseNumeratorDenseDenominatorDotRightDivide<T>(dataClass, A, B);
    }
    if (!B.isScalar()) {
        return sparseNumeratorDotRightDivideMatrix<T>(dataClass, A, B);
    }
    T scalarValue = getSparseScalarValue<T>(B);
    if (scalarValue == T(0)) {
        return sparseNumeratorDotRightDivideScalarZero<T>(dataClass, A, A.dimensions());
    }
    return scalarSparseOperatorProduct<T>(dataClass, A.dimensions(), A, T(1) / scalarValue);
}
//=============================================================================
static ArrayOf
sparseDotRightDivide(const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    if (A.dataClass() != B.dataClass()) {
        needToOverload = true;
        return {};
    }
    switch (A.dataClass()) {
    case NLS_DOUBLE:
        if (A.isSparse()) {
            return sparseDotRightDivide<double>(NLS_DOUBLE, A, B, needToOverload);
        }
        needToOverload = false;
        return denseNumeratorSparseDenominatorDotRightDivide<double>(NLS_DOUBLE, A, B);
    case NLS_SINGLE:
        if (A.isSparse()) {
            return sparseDotRightDivide<single>(NLS_SINGLE, A, B, needToOverload);
        }
        needToOverload = false;
        return denseNumeratorSparseDenominatorDotRightDivide<single>(NLS_SINGLE, A, B);
    case NLS_DCOMPLEX:
        if (A.isSparse()) {
            return sparseDotRightDivide<doublecomplex>(NLS_DCOMPLEX, A, B, needToOverload);
        }
        needToOverload = false;
        return denseNumeratorSparseDenominatorDotRightDivide<doublecomplex>(NLS_DCOMPLEX, A, B);
    case NLS_SCOMPLEX:
        if (A.isSparse()) {
            return sparseDotRightDivide<singlecomplex>(NLS_SCOMPLEX, A, B, needToOverload);
        }
        needToOverload = false;
        return denseNumeratorSparseDenominatorDotRightDivide<singlecomplex>(NLS_SCOMPLEX, A, B);
    default:
        needToOverload = true;
        return {};
    }
}
//=============================================================================
bool
dotRightDivideComplexRealScaling(const ArrayOf& numerator, const ArrayOf& divisor, ArrayOf& result)
{
    if (numerator.isSparse() || divisor.isSparse() || numerator.isEmpty() || divisor.isEmpty()) {
        return false;
    }
    NelsonType numeratorClass = numerator.dataClass();
    NelsonType divisorClass = divisor.dataClass();
    bool handled = false;
    if (numeratorClass == NLS_DCOMPLEX && divisorClass == NLS_DOUBLE) {
        handled
            = complex_real_dotRightDivide_scaling<double>(NLS_DCOMPLEX, numerator, divisor, result);
        if (handled && result.allReal()) {
            result.promoteType(NLS_DOUBLE);
        }
    } else if (numeratorClass == NLS_SCOMPLEX && divisorClass == NLS_SINGLE) {
        handled
            = complex_real_dotRightDivide_scaling<single>(NLS_SCOMPLEX, numerator, divisor, result);
        if (handled && result.allReal()) {
            result.promoteType(NLS_SINGLE);
        }
    }
    return handled;
}
//=============================================================================
ArrayOf
DotRightDivide(const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    ArrayOf res;
    if (A.isEmpty() || B.isEmpty()) {
        if (A.isScalar() || B.isScalar()) {
            if (A.isScalar()) {
                res = B;
            } else {
                res = A;
            }
        } else {
            res = binaryOperatorEmptyMatrixEmptyMatrix(
                A, B, A.dataClass() > B.dataClass() ? A.dataClass() : B.dataClass(), "./");
        }
        if (res.dataClass() == NLS_DCOMPLEX) {
            if (res.allReal()) {
                res.promoteType(NLS_DOUBLE);
            }
        }
        if (res.dataClass() == NLS_SCOMPLEX) {
            if (res.allReal()) {
                res.promoteType(NLS_SINGLE);
            }
        }
        return res;
    }
    if (A.isSparse() || B.isSparse()) {
        return sparseDotRightDivide(A, B, needToOverload);
    }

    if (A.dataClass() != B.dataClass()) {
        if ((A.isDoubleClass() && B.isSingleClass()) || (A.isSingleClass() && B.isDoubleClass())) {
            bool isComplex = A.isComplex() || B.isComplex();
            ArrayOf AA = A;
            ArrayOf BB = B;
            if (isComplex) {
                AA.promoteType(NLS_SCOMPLEX);
                BB.promoteType(NLS_SCOMPLEX);
            } else {
                AA.promoteType(NLS_SINGLE);
                BB.promoteType(NLS_SINGLE);
            }
            return DotRightDivide(AA, BB, needToOverload);
        }

        if (A.isIntegerType()) {
            bool isCompatible = isScalarDoubleOrChar(B);
            if (!isCompatible) {
                Error(L"Nelson:operators:integerClassMismatch",
                    L"Integers can only be combined with integers of the same class, or scalar "
                    L"doubles.");
            }
            if (B.dataClass() == NLS_CHAR) {
                ArrayOf BB = B;
                BB.promoteType(A.dataClass());
                return DotRightDivide(A, BB, needToOverload);
            }
            if (B.isComplex()) {
                Error(L"Nelson:operators:complexIntegerNotAllowed",
                    L"Complex integer not allowed for arithmetic operator *");
            }
            ArrayOf AA = A;
            AA.promoteType(B.dataClass());
            ArrayOf res = DotRightDivide(AA, B, needToOverload);
            if (!needToOverload) {
                res.promoteType(A.dataClass());
            }
            return res;
        } else if (B.isIntegerType()) {
            bool isCompatible = isScalarDoubleOrChar(A);
            if (!isCompatible) {
                Error(L"Nelson:operators:integerClassMismatch",
                    L"Integers can only be combined with integers of the same class, or scalar "
                    L"doubles.");
            }
            if (A.dataClass() == NLS_CHAR) {
                ArrayOf AA = A;
                AA.promoteType(B.dataClass());
                return DotRightDivide(AA, B, needToOverload);
            }
            if (A.isComplex()) {
                Error(L"Nelson:operators:complexIntegerNotAllowed",
                    L"Complex integer not allowed for arithmetic operator *");
            }
            ArrayOf BB = B;
            BB.promoteType(A.dataClass());
            ArrayOf res = DotRightDivide(A, BB, needToOverload);
            if (!needToOverload) {
                res.promoteType(B.dataClass());
            }
            return res;
        }
    }
    NelsonType commonType = A.dataClass();

    switch (commonType) {
    case NLS_INT8:
        return integer_dotRightDivide<int8>(NLS_INT8, A, B);
    case NLS_UINT8:
        return integer_dotRightDivide<uint8>(NLS_UINT8, A, B);
    case NLS_INT16:
        return integer_dotRightDivide<int16>(NLS_INT16, A, B);
    case NLS_UINT16:
        return integer_dotRightDivide<uint16>(NLS_UINT16, A, B);
    case NLS_INT32:
        return integer_dotRightDivide<int32>(NLS_INT32, A, B);
    case NLS_UINT32:
        return integer_dotRightDivide<uint32>(NLS_UINT32, A, B);
    case NLS_INT64:
        return integer_dotRightDivide<int64>(NLS_INT64, A, B);
    case NLS_UINT64:
        return integer_dotRightDivide<uint64>(NLS_UINT64, A, B);
    case NLS_SINGLE: {
        res = real_dotRightDivide<single>(NLS_SINGLE, A, B);
    } break;
    case NLS_DOUBLE: {
        res = real_dotRightDivide<double>(NLS_DOUBLE, A, B);
    } break;
    case NLS_SCOMPLEX: {
        res = complex_dotRightDivide<single>(NLS_SCOMPLEX, A, B);
        if (res.allReal()) {
            res.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_DCOMPLEX: {
        res = complex_dotRightDivide<double>(NLS_DCOMPLEX, A, B);
        if (res.allReal()) {
            res.promoteType(NLS_DOUBLE);
        }
    } break;
    default:
        needToOverload = true;
        break;
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
