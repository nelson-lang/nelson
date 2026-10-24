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
#include <cmath>
#include <unordered_set>
#include <vector>
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "Equals.hpp"
#include "BinaryOperatorsHelpers.hpp"
#include "MatrixCheck.hpp"
#include "NewWithException.hpp"
#include "SparseOperatorsHelpers.hpp"
#include "RelationalOperatorMixed64.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
static double
sparseRelationOrderValue(const T& value)
{
    return static_cast<double>(value);
}
//=============================================================================
template <>
double
sparseRelationOrderValue<doublecomplex>(const doublecomplex& value)
{
    // ordered comparisons use the real part only
    return value.real();
}
//=============================================================================
template <>
double
sparseRelationOrderValue<singlecomplex>(const singlecomplex& value)
{
    // ordered comparisons use the real part only
    return static_cast<double>(value.real());
}
//=============================================================================
template <class T>
static bool
sparseRelationCompare(const T& a, const T& b, SparseRelationOperation operation)
{
    switch (operation) {
    case SparseRelationOperation::Equal:
        return a == b;
    case SparseRelationOperation::NotEqual:
        return a != b;
    case SparseRelationOperation::Less:
        return sparseRelationOrderValue<T>(a) < sparseRelationOrderValue<T>(b);
    case SparseRelationOperation::LessEqual:
        return sparseRelationOrderValue<T>(a) <= sparseRelationOrderValue<T>(b);
    case SparseRelationOperation::Greater:
        return sparseRelationOrderValue<T>(a) > sparseRelationOrderValue<T>(b);
    case SparseRelationOperation::GreaterEqual:
        return sparseRelationOrderValue<T>(a) >= sparseRelationOrderValue<T>(b);
    }
    return false;
}
//=============================================================================
template <class T>
static ArrayOf
makeSparseRelationResult(const Dimensions& dimensions,
    const std::vector<Eigen::Triplet<logical, signedIndexType>>& values)
{
    auto* sparseResult = new NelsonSparseMatrix<logical>(dimensions.rows(), dimensions.columns());
    if (!values.empty()) {
        sparseResult->setFromTriplets(values.begin(), values.end());
    }
    return makeSparseOperatorResult<logical>(NLS_LOGICAL, dimensions, sparseResult);
}
//=============================================================================
template <class T>
static void
appendSparseRelationValue(std::vector<Eigen::Triplet<logical, signedIndexType>>& values,
    indexType rows, indexType linearIndex, const T& a, const T& b,
    SparseRelationOperation operation);
//=============================================================================
template <class T>
static T
sparseRelationValue(const ArrayOf& A, const NelsonSparseMatrix<T>* sparseA, const T* denseA,
    indexType row, indexType col, indexType linearIndex)
{
    if (sparseA) {
        signedIndexType sparseRow = A.isScalar() ? 0 : static_cast<signedIndexType>(row);
        signedIndexType sparseCol = A.isScalar() ? 0 : static_cast<signedIndexType>(col);
        return sparseA->coeff(sparseRow, sparseCol);
    }
    return denseA[A.isScalar() ? 0 : linearIndex];
}
//=============================================================================
template <class T>
static ArrayOf
sparseRelationFullIteration(
    const ArrayOf& A, const ArrayOf& B, const Dimensions& dims, SparseRelationOperation operation)
{
    indexType rows = dims.rows();
    indexType cols = dims.columns();
    const auto* sparseA = A.isSparse() ? getSparseMatrixPointer<T>(A) : nullptr;
    const auto* sparseB = B.isSparse() ? getSparseMatrixPointer<T>(B) : nullptr;
    const T* denseA = A.isSparse() ? nullptr : static_cast<const T*>(A.dataPointer());
    const T* denseB = B.isSparse() ? nullptr : static_cast<const T*>(B.dataPointer());
    std::vector<Eigen::Triplet<logical, signedIndexType>> values;
    values.reserve(dims.elementCount());
    for (indexType col = 0; col < cols; ++col) {
        for (indexType row = 0; row < rows; ++row) {
            indexType linearIndex = row + col * rows;
            T a = sparseRelationValue<T>(A, sparseA, denseA, row, col, linearIndex);
            T b = sparseRelationValue<T>(B, sparseB, denseB, row, col, linearIndex);
            appendSparseRelationValue<T>(values, rows, linearIndex, a, b, operation);
        }
    }
    return makeSparseRelationResult<T>(dims, values);
}
//=============================================================================
template <class T>
static ArrayOf
sparseRelationBroadcast(const ArrayOf& A, const ArrayOf& B, SparseRelationOperation operation)
{
    BinaryBroadcast info = binaryOperatorBroadcast(A, B, "relational");
    indexType rows = info.dimensions.rows();
    const auto* sparseA = getSparseMatrixPointer<T>(A);
    const auto* sparseB = getSparseMatrixPointer<T>(B);
    std::vector<Eigen::Triplet<logical, signedIndexType>> values;
    values.reserve(info.dimensions.elementCount());
    for (indexType idx = 0; idx < info.dimensions.elementCount(); ++idx) {
        indexType indexA = binaryOperatorBroadcastIndex(info, idx, info.aStrides);
        indexType indexB = binaryOperatorBroadcastIndex(info, idx, info.bStrides);
        T a = sparseA->coeff(static_cast<signedIndexType>(indexA % A.rows()),
            static_cast<signedIndexType>(indexA / A.rows()));
        T b = sparseB->coeff(static_cast<signedIndexType>(indexB % B.rows()),
            static_cast<signedIndexType>(indexB / B.rows()));
        appendSparseRelationValue<T>(values, rows, idx, a, b, operation);
    }
    return makeSparseRelationResult<T>(info.dimensions, values);
}
//=============================================================================
template <class T>
static void
appendSparseRelationValue(std::vector<Eigen::Triplet<logical, signedIndexType>>& values,
    indexType rows, indexType linearIndex, const T& a, const T& b,
    SparseRelationOperation operation)
{
    if (sparseRelationCompare<T>(a, b, operation)) {
        values.emplace_back(static_cast<signedIndexType>(linearIndex % rows),
            static_cast<signedIndexType>(linearIndex / rows), static_cast<logical>(true));
    }
}
//=============================================================================
template <class T>
static ArrayOf
sparseRelationSameSize(const ArrayOf& A, const ArrayOf& B, SparseRelationOperation operation)
{
    Dimensions dims = A.dimensions();
    indexType rows = dims.rows();
    indexType cols = dims.columns();
    const auto* sparseA = getSparseMatrixPointer<T>(A);
    const auto* sparseB = getSparseMatrixPointer<T>(B);
    std::vector<Eigen::Triplet<logical, signedIndexType>> values;
    T zero = T(0);
    if (sparseRelationCompare<T>(zero, zero, operation)) {
        // 0 OP 0 is true, so the result is almost entirely true. Scatter each column's
        // stored values into dense row buffers: O(rows*cols) instead of a coeff() binary
        // search per cell, O(rows*cols*log nnz).
        values.reserve(dims.elementCount());
        std::vector<T> columnA(rows, zero);
        std::vector<T> columnB(rows, zero);
        for (indexType col = 0; col < cols; ++col) {
            std::fill(columnA.begin(), columnA.end(), zero);
            std::fill(columnB.begin(), columnB.end(), zero);
            for (typename NelsonSparseMatrix<T>::InnerIterator it(
                     *sparseA, static_cast<signedIndexType>(col));
                it; ++it) {
                columnA[static_cast<indexType>(it.row())] = it.value();
            }
            for (typename NelsonSparseMatrix<T>::InnerIterator it(
                     *sparseB, static_cast<signedIndexType>(col));
                it; ++it) {
                columnB[static_cast<indexType>(it.row())] = it.value();
            }
            for (indexType row = 0; row < rows; ++row) {
                appendSparseRelationValue<T>(
                    values, rows, row + col * rows, columnA[row], columnB[row], operation);
            }
        }
    } else {
        std::unordered_set<indexType> candidateIndices;
        candidateIndices.reserve(sparseA->nonZeros() + sparseB->nonZeros());
        for (signedIndexType col = 0; col < sparseA->outerSize(); ++col) {
            for (typename NelsonSparseMatrix<T>::InnerIterator it(*sparseA, col); it; ++it) {
                candidateIndices.insert(static_cast<indexType>(it.row() + it.col() * rows));
            }
        }
        for (signedIndexType col = 0; col < sparseB->outerSize(); ++col) {
            for (typename NelsonSparseMatrix<T>::InnerIterator it(*sparseB, col); it; ++it) {
                candidateIndices.insert(static_cast<indexType>(it.row() + it.col() * rows));
            }
        }
        values.reserve(candidateIndices.size());
        for (indexType linearIndex : candidateIndices) {
            indexType row = linearIndex % rows;
            indexType col = linearIndex / rows;
            appendSparseRelationValue<T>(values, rows, linearIndex,
                sparseA->coeff(
                    static_cast<signedIndexType>(row), static_cast<signedIndexType>(col)),
                sparseB->coeff(
                    static_cast<signedIndexType>(row), static_cast<signedIndexType>(col)),
                operation);
        }
    }
    return makeSparseRelationResult<T>(dims, values);
}
//=============================================================================
// Reads a scalar operand stored sparse or dense, so `sparse OP denseScalar` (A > 0)
// can use the fast matrix-scalar paths.
template <class T>
static T
relationScalarValue(const ArrayOf& A)
{
    if (A.isSparse()) {
        return getSparseMatrixPointer<T>(A)->coeff(0, 0);
    }
    return *static_cast<const T*>(A.dataPointer());
}
//=============================================================================
template <class T>
static ArrayOf
sparseRelationMatrixScalar(const ArrayOf& A, const ArrayOf& B, SparseRelationOperation operation)
{
    Dimensions dims = A.dimensions();
    indexType rows = dims.rows();
    indexType cols = dims.columns();
    const auto* sparseA = getSparseMatrixPointer<T>(A);
    T scalarValue = relationScalarValue<T>(B);
    T zero = T(0);
    std::vector<Eigen::Triplet<logical, signedIndexType>> values;
    if (sparseRelationCompare<T>(zero, scalarValue, operation)) {
        values.reserve(dims.elementCount());
        for (indexType col = 0; col < cols; ++col) {
            for (indexType row = 0; row < rows; ++row) {
                appendSparseRelationValue<T>(values, rows, row + col * rows,
                    sparseA->coeff(
                        static_cast<signedIndexType>(row), static_cast<signedIndexType>(col)),
                    scalarValue, operation);
            }
        }
    } else {
        values.reserve(sparseA->nonZeros());
        for (signedIndexType col = 0; col < sparseA->outerSize(); ++col) {
            for (typename NelsonSparseMatrix<T>::InnerIterator it(*sparseA, col); it; ++it) {
                appendSparseRelationValue<T>(values, rows,
                    static_cast<indexType>(it.row() + it.col() * rows), it.value(), scalarValue,
                    operation);
            }
        }
    }
    return makeSparseRelationResult<T>(dims, values);
}
//=============================================================================
template <class T>
static ArrayOf
sparseRelationScalarMatrix(const ArrayOf& A, const ArrayOf& B, SparseRelationOperation operation)
{
    Dimensions dims = B.dimensions();
    indexType rows = dims.rows();
    indexType cols = dims.columns();
    T scalarValue = relationScalarValue<T>(A);
    const auto* sparseB = getSparseMatrixPointer<T>(B);
    T zero = T(0);
    std::vector<Eigen::Triplet<logical, signedIndexType>> values;
    if (sparseRelationCompare<T>(scalarValue, zero, operation)) {
        values.reserve(dims.elementCount());
        for (indexType col = 0; col < cols; ++col) {
            for (indexType row = 0; row < rows; ++row) {
                appendSparseRelationValue<T>(values, rows, row + col * rows, scalarValue,
                    sparseB->coeff(
                        static_cast<signedIndexType>(row), static_cast<signedIndexType>(col)),
                    operation);
            }
        }
    } else {
        values.reserve(sparseB->nonZeros());
        for (signedIndexType col = 0; col < sparseB->outerSize(); ++col) {
            for (typename NelsonSparseMatrix<T>::InnerIterator it(*sparseB, col); it; ++it) {
                appendSparseRelationValue<T>(values, rows,
                    static_cast<indexType>(it.row() + it.col() * rows), scalarValue, it.value(),
                    operation);
            }
        }
    }
    return makeSparseRelationResult<T>(dims, values);
}
//=============================================================================
template <class T>
static ArrayOf
sparseRelationOperator(
    const ArrayOf& A, const ArrayOf& B, const std::wstring& operatorName, bool& needToOverload)
{
    needToOverload = false;
    SparseRelationOperation operation = sparseRelationOperationFromName(operatorName);
    Dimensions dimsA = A.dimensions();
    Dimensions dimsB = B.dimensions();
    if (SameSizeCheck(dimsA, dimsB)) {
        if (!A.isSparse() || !B.isSparse()) {
            return sparseRelationFullIteration<T>(A, B, dimsA, operation);
        }
        return sparseRelationSameSize<T>(A, B, operation);
    }
    if (A.isScalar()) {
        // When the matrix operand B is sparse, use the fast nnz-only path even
        // if the scalar A is dense (the common `scalar OP sparse` case).
        if (B.isSparse()) {
            return sparseRelationScalarMatrix<T>(A, B, operation);
        }
        return sparseRelationFullIteration<T>(A, B, dimsB, operation);
    }
    if (B.isScalar()) {
        // Likewise for `sparse OP scalar` (e.g. A > 0): the scalar is dense but
        // only A's stored nonzeros can matter when 0 OP scalar is false.
        if (A.isSparse()) {
            return sparseRelationMatrixScalar<T>(A, B, operation);
        }
        return sparseRelationFullIteration<T>(A, B, dimsA, operation);
    }
    if (A.isSparse() && B.isSparse()) {
        return sparseRelationBroadcast<T>(A, B, operation);
    }
    needToOverload = true;
    return {};
}
//=============================================================================
static ArrayOf
sparseRelationOperator(
    const ArrayOf& A, const ArrayOf& B, const std::wstring& operatorName, bool& needToOverload)
{
    if ((!A.isSparse() && !B.isSparse()) || A.dataClass() != B.dataClass()) {
        needToOverload = true;
        return {};
    }
    switch (A.dataClass()) {
    case NLS_LOGICAL:
        return sparseRelationOperator<logical>(A, B, operatorName, needToOverload);
    case NLS_SINGLE:
        return sparseRelationOperator<single>(A, B, operatorName, needToOverload);
    case NLS_DOUBLE:
        return sparseRelationOperator<double>(A, B, operatorName, needToOverload);
    case NLS_SCOMPLEX:
        return sparseRelationOperator<singlecomplex>(A, B, operatorName, needToOverload);
    case NLS_DCOMPLEX:
        return sparseRelationOperator<doublecomplex>(A, B, operatorName, needToOverload);
    default:
        needToOverload = true;
        return {};
    }
}
//=============================================================================
static bool
tryDoubleScalarRelation(
    const ArrayOf& A, const ArrayOf& B, const std::wstring& operatorName, ArrayOf& result)
{
    if (A.dataClass() != NLS_DOUBLE || B.dataClass() != NLS_DOUBLE || A.isSparse() || B.isSparse()
        || (!A.isScalar() && !B.isScalar())
        || !(operatorName == L"<" || operatorName == L"<=" || operatorName == L">"
            || operatorName == L">=" || operatorName == L"==" || operatorName == L"~=")) {
        return false;
    }
    const bool scalarLeft = A.isScalar();
    const ArrayOf& arrayValue = scalarLeft ? B : A;
    const Dimensions dimsC = arrayValue.dimensions();
    const indexType Clen = dimsC.elementCount();
    auto* Cp = new_with_exception<logical>(Clen, false);
    const auto* ptrA = static_cast<const double*>(A.dataPointer());
    const auto* ptrB = static_cast<const double*>(B.dataPointer());
    SparseRelationOperation operation = sparseRelationOperationFromName(operatorName);
    if (scalarLeft) {
        const double scalar = ptrA[0];
        switch (operation) {
        case SparseRelationOperation::Less:
            OMP_PARALLEL_FOR_LOOP(Clen)
            for (ompIndexType i = 0; i < static_cast<ompIndexType>(Clen); ++i) {
                Cp[i] = static_cast<logical>(scalar < ptrB[i]);
            }
            break;
        case SparseRelationOperation::LessEqual:
            OMP_PARALLEL_FOR_LOOP(Clen)
            for (ompIndexType i = 0; i < static_cast<ompIndexType>(Clen); ++i) {
                Cp[i] = static_cast<logical>(scalar <= ptrB[i]);
            }
            break;
        case SparseRelationOperation::Greater:
            OMP_PARALLEL_FOR_LOOP(Clen)
            for (ompIndexType i = 0; i < static_cast<ompIndexType>(Clen); ++i) {
                Cp[i] = static_cast<logical>(scalar > ptrB[i]);
            }
            break;
        case SparseRelationOperation::GreaterEqual:
            OMP_PARALLEL_FOR_LOOP(Clen)
            for (ompIndexType i = 0; i < static_cast<ompIndexType>(Clen); ++i) {
                Cp[i] = static_cast<logical>(scalar >= ptrB[i]);
            }
            break;
        case SparseRelationOperation::Equal:
            OMP_PARALLEL_FOR_LOOP(Clen)
            for (ompIndexType i = 0; i < static_cast<ompIndexType>(Clen); ++i) {
                Cp[i] = static_cast<logical>(scalar == ptrB[i]);
            }
            break;
        case SparseRelationOperation::NotEqual:
            OMP_PARALLEL_FOR_LOOP(Clen)
            for (ompIndexType i = 0; i < static_cast<ompIndexType>(Clen); ++i) {
                Cp[i] = static_cast<logical>(scalar != ptrB[i]);
            }
            break;
        }
    } else {
        const double scalar = ptrB[0];
        switch (operation) {
        case SparseRelationOperation::Less:
            OMP_PARALLEL_FOR_LOOP(Clen)
            for (ompIndexType i = 0; i < static_cast<ompIndexType>(Clen); ++i) {
                Cp[i] = static_cast<logical>(ptrA[i] < scalar);
            }
            break;
        case SparseRelationOperation::LessEqual:
            OMP_PARALLEL_FOR_LOOP(Clen)
            for (ompIndexType i = 0; i < static_cast<ompIndexType>(Clen); ++i) {
                Cp[i] = static_cast<logical>(ptrA[i] <= scalar);
            }
            break;
        case SparseRelationOperation::Greater:
            OMP_PARALLEL_FOR_LOOP(Clen)
            for (ompIndexType i = 0; i < static_cast<ompIndexType>(Clen); ++i) {
                Cp[i] = static_cast<logical>(ptrA[i] > scalar);
            }
            break;
        case SparseRelationOperation::GreaterEqual:
            OMP_PARALLEL_FOR_LOOP(Clen)
            for (ompIndexType i = 0; i < static_cast<ompIndexType>(Clen); ++i) {
                Cp[i] = static_cast<logical>(ptrA[i] >= scalar);
            }
            break;
        case SparseRelationOperation::Equal:
            OMP_PARALLEL_FOR_LOOP(Clen)
            for (ompIndexType i = 0; i < static_cast<ompIndexType>(Clen); ++i) {
                Cp[i] = static_cast<logical>(ptrA[i] == scalar);
            }
            break;
        case SparseRelationOperation::NotEqual:
            OMP_PARALLEL_FOR_LOOP(Clen)
            for (ompIndexType i = 0; i < static_cast<ompIndexType>(Clen); ++i) {
                Cp[i] = static_cast<logical>(ptrA[i] != scalar);
            }
            break;
        }
    }
    result = ArrayOf(NLS_LOGICAL, dimsC, Cp, false);
    return true;
}
//=============================================================================
static ArrayOf
matrix_matrix_operator(const ArrayOf& A, const ArrayOf& B,
    logical (*realRelationOperator)(
        NelsonType commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB),
    logical (*complexRelationOperator)(
        NelsonType commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB),
    logical (*stringRelationOperator)(
        NelsonType commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB))
{
    ArrayOf res;
    Dimensions dimsC = A.dimensions();
    indexType Clen = dimsC.elementCount();
    auto* Cp = new_with_exception<logical>(Clen, false);

    void* ptrA = const_cast<void*>(A.dataPointer());
    void* ptrB = const_cast<void*>(B.dataPointer());
    NelsonType classA = A.dataClass();

    logical (*relationOperator)(NelsonType, void*, void*, indexType, indexType) = nullptr;

    switch (classA) {
    case NLS_STRING_ARRAY:
        relationOperator = stringRelationOperator;
        break;
    case NLS_CHAR:
    case NLS_LOGICAL:
    case NLS_UINT8:
    case NLS_INT8:
    case NLS_UINT16:
    case NLS_INT16:
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_UINT64:
    case NLS_INT64:
    case NLS_SINGLE:
    case NLS_DOUBLE:
        relationOperator = realRelationOperator;
        break;
    case NLS_SCOMPLEX:
    case NLS_DCOMPLEX:
        relationOperator = complexRelationOperator;
        break;
    default:
        break;
    }

    if (relationOperator) {
        if (Clen == 1) {
            Cp[0] = relationOperator(classA, ptrA, ptrB, 0, 0);
        } else {
            OMP_PARALLEL_FOR_LOOP(Clen)
            for (ompIndexType i = 0; i < (ompIndexType)Clen; i++) {
                Cp[i] = relationOperator(classA, ptrA, ptrB, i, i);
            }
        }
    }
    return ArrayOf(NLS_LOGICAL, dimsC, Cp, false);
}
//=============================================================================
static ArrayOf
scalar_matrix_operator(const ArrayOf& A, const ArrayOf& B,
    logical (*realRelationOperator)(
        NelsonType commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB),
    logical (*complexRelationOperator)(
        NelsonType commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB),
    logical (*stringRelationOperator)(
        NelsonType commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB))
{
    Dimensions dimsC = B.dimensions();
    indexType Clen = dimsC.elementCount();
    auto* Cp = new_with_exception<logical>(Clen, false);
    void* ptrA = const_cast<void*>(A.dataPointer());
    void* ptrB = const_cast<void*>(B.dataPointer());
    NelsonType classA = A.dataClass();

    logical (*relationOperator)(NelsonType, void*, void*, indexType, indexType) = nullptr;

    switch (classA) {
    case NLS_STRING_ARRAY:
        relationOperator = stringRelationOperator;
        break;
    case NLS_CHAR:
    case NLS_LOGICAL:
    case NLS_UINT8:
    case NLS_INT8:
    case NLS_UINT16:
    case NLS_INT16:
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_UINT64:
    case NLS_INT64:
    case NLS_SINGLE:
    case NLS_DOUBLE:
        relationOperator = realRelationOperator;
        break;
    case NLS_SCOMPLEX:
    case NLS_DCOMPLEX:
        relationOperator = complexRelationOperator;
        break;
    default:
        break;
    }
    if (relationOperator) {
        OMP_PARALLEL_FOR_LOOP(Clen)
        for (ompIndexType i = 0; i < (ompIndexType)Clen; i++) {
            Cp[i] = relationOperator(classA, ptrA, ptrB, 0, i);
        }
    }
    return ArrayOf(NLS_LOGICAL, dimsC, Cp, false);
}
//=============================================================================
static ArrayOf
matrix_scalar_operator(const ArrayOf& A, const ArrayOf& B,
    logical (*realRelationOperator)(
        NelsonType commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB),
    logical (*complexRelationOperator)(
        NelsonType commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB),
    logical (*stringRelationOperator)(
        NelsonType commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB))
{
    Dimensions dimsC = A.dimensions();
    indexType Clen = dimsC.elementCount();
    auto* Cp = new_with_exception<logical>(Clen, false);
    void* ptrA = const_cast<void*>(A.dataPointer());
    void* ptrB = const_cast<void*>(B.dataPointer());
    NelsonType classA = A.dataClass();
    logical (*relationOperator)(NelsonType, void*, void*, indexType, indexType) = nullptr;

    switch (classA) {
    case NLS_STRING_ARRAY:
        relationOperator = stringRelationOperator;
        break;
    case NLS_CHAR:
    case NLS_LOGICAL:
    case NLS_UINT8:
    case NLS_INT8:
    case NLS_UINT16:
    case NLS_INT16:
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_UINT64:
    case NLS_INT64:
    case NLS_SINGLE:
    case NLS_DOUBLE:
        relationOperator = realRelationOperator;
        break;
    case NLS_SCOMPLEX:
    case NLS_DCOMPLEX:
        relationOperator = complexRelationOperator;
        break;
    default:
        break;
    }
    if (relationOperator) {
        OMP_PARALLEL_FOR_LOOP(Clen)
        for (ompIndexType i = 0; i < (ompIndexType)Clen; i++) {
            Cp[i] = relationOperator(classA, ptrA, ptrB, i, 0);
        }
    }
    return ArrayOf(NLS_LOGICAL, dimsC, Cp, false);
}
//=============================================================================
static ArrayOf
broadcast_operator(const ArrayOf& A, const ArrayOf& B, const std::wstring& operatorName,
    logical (*realRelationOperator)(
        NelsonType commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB),
    logical (*complexRelationOperator)(
        NelsonType commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB),
    logical (*stringRelationOperator)(
        NelsonType commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB))
{
    BinaryBroadcast info = binaryOperatorBroadcast(A, B, asciiOperatorName(operatorName));
    indexType Clen = info.dimensions.elementCount();
    auto* Cp = new_with_exception<logical>(Clen, false);
    void* ptrA = const_cast<void*>(A.dataPointer());
    void* ptrB = const_cast<void*>(B.dataPointer());
    NelsonType classA = A.dataClass();
    logical (*relationOperator)(NelsonType, void*, void*, indexType, indexType) = nullptr;

    switch (classA) {
    case NLS_STRING_ARRAY:
        relationOperator = stringRelationOperator;
        break;
    case NLS_CHAR:
    case NLS_LOGICAL:
    case NLS_UINT8:
    case NLS_INT8:
    case NLS_UINT16:
    case NLS_INT16:
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_UINT64:
    case NLS_INT64:
    case NLS_SINGLE:
    case NLS_DOUBLE:
        relationOperator = realRelationOperator;
        break;
    case NLS_SCOMPLEX:
    case NLS_DCOMPLEX:
        relationOperator = complexRelationOperator;
        break;
    default:
        break;
    }
    if (relationOperator) {
        for (indexType i = 0; i < Clen; ++i) {
            indexType indexA = binaryOperatorBroadcastIndex(info, i, info.aStrides);
            indexType indexB = binaryOperatorBroadcastIndex(info, i, info.bStrides);
            Cp[i] = relationOperator(classA, ptrA, ptrB, indexA, indexB);
        }
    }
    return ArrayOf(NLS_LOGICAL, info.dimensions, Cp, false);
}
//=============================================================================
ArrayOf
relationOperator(const ArrayOf& A, const ArrayOf& B, const std::wstring& operatorName,
    logical (*realRelationOperator)(
        NelsonType commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB),
    logical (*complexRelationOperator)(
        NelsonType commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB),
    logical (*stringRelationOperator)(
        NelsonType commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB),
    bool& needToOverload)
{
    Dimensions dimsA = A.dimensions();
    Dimensions dimsB = B.dimensions();
    if (A.isEmpty() || B.isEmpty()) {
        if (A.isScalar() || B.isScalar()) {
            if (A.isScalar()) {
                ArrayOf res = ArrayOf::empty(dimsB);
                res.promoteType(NLS_LOGICAL);
                return res;
            }
            ArrayOf res = ArrayOf::empty(dimsA);
            res.promoteType(NLS_LOGICAL);
            return res;
        }
        BinaryBroadcast info = binaryOperatorBroadcast(A, B, asciiOperatorName(operatorName));
        ArrayOf res = ArrayOf::empty(info.dimensions);
        res.promoteType(NLS_LOGICAL);
        return res;
    }
    if (A.isSparse() || B.isSparse()) {
        return sparseRelationOperator(A, B, operatorName, needToOverload);
    }
    if (A.dataClass() != B.dataClass() && isMixed64BitIntegerDoublePair(A, B)) {
        return mixed64BitIntegerDoubleRelation(A, B, operatorName);
    }
    ArrayOf scalarRelationResult;
    if (tryDoubleScalarRelation(A, B, operatorName, scalarRelationResult)) {
        return scalarRelationResult;
    }
    if (SameSizeCheck(dimsA, dimsB)) {
        return matrix_matrix_operator(
            A, B, realRelationOperator, complexRelationOperator, stringRelationOperator);
    }
    if (A.isScalar() || B.isScalar()) {
        if (A.isScalar()) {
            return scalar_matrix_operator(
                A, B, realRelationOperator, complexRelationOperator, stringRelationOperator);
        }
        return matrix_scalar_operator(
            A, B, realRelationOperator, complexRelationOperator, stringRelationOperator);
    }
    return broadcast_operator(
        A, B, operatorName, realRelationOperator, complexRelationOperator, stringRelationOperator);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
