//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "omp_for_loop.hpp"
#include "Maximum.hpp"
#include "ExtremumOfEmpty.hpp"
#include "ExtremumKernels.hpp"
#include "SparseExtremumSparsity.hpp"
#include "complex_abs.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "SparseType.hpp"
#include "SparseArrayOf.hpp"
#include "BinaryOperatorsHelpers.hpp"
#include <Eigen/Sparse>
#include <unordered_set>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
static bool
sparseMaxBetter(T candidate, T current, bool omitNaN)
{
    bool candidateNaN = std::isnan((double)candidate);
    bool currentNaN = std::isnan((double)current);
    if (omitNaN && candidateNaN) {
        return false;
    }
    if (!omitNaN && candidateNaN) {
        return true;
    }
    if (currentNaN) {
        return !candidateNaN;
    }
    return candidate > current;
}
//=============================================================================
template <class T>
static ArrayOfVector
SparseMaximumTemplate(const ArrayOf& A, indexType dim, int nLhs, bool omitNaN)
{
    using SparseMatrix = Eigen::SparseMatrix<T, 0, signedIndexType>;
    indexType rows = A.rows();
    indexType cols = A.columns();
    indexType workDim = dim - 1;
    Dimensions outDims = workDim == 0 ? Dimensions(1, cols) : Dimensions(rows, 1);
    std::vector<T> values(workDim == 0 ? cols : rows, T(0));
    std::vector<double> indices(workDim == 0 ? cols : rows, 1);
    std::vector<bool> initialized(values.size(), false);

    const auto* spmatSRC = static_cast<const SparseMatrix*>(A.sparseDataPointer());
    if (workDim == 0) {
        for (indexType col = 0; col < cols; ++col) {
            signedIndexType expectedRow = 0;
            bool hasImplicitZero = false;
            double firstZero = 1;
            for (typename SparseMatrix::InnerIterator it(*spmatSRC, (signedIndexType)col); it;
                ++it) {
                if (!hasImplicitZero && it.row() > expectedRow) {
                    hasImplicitZero = true;
                    firstZero = (double)expectedRow + 1;
                }
                expectedRow = it.row() + 1;
            }
            if (!hasImplicitZero && expectedRow < (signedIndexType)rows) {
                hasImplicitZero = true;
                firstZero = (double)expectedRow + 1;
            }
            if (hasImplicitZero) {
                initialized[col] = true;
                values[col] = T(0);
                indices[col] = firstZero;
            }
            for (typename SparseMatrix::InnerIterator it(*spmatSRC, (signedIndexType)col); it;
                ++it) {
                if (!initialized[col] || sparseMaxBetter<T>(it.value(), values[col], omitNaN)) {
                    initialized[col] = true;
                    values[col] = it.value();
                    indices[col] = (double)it.row() + 1;
                }
            }
        }
    } else {
        std::vector<indexType> counts(rows, 0);
        std::vector<signedIndexType> nextExpected(rows, 0);
        std::vector<double> firstZero(rows, 0);
        for (signedIndexType col = 0; col < spmatSRC->outerSize(); ++col) {
            for (typename SparseMatrix::InnerIterator it(*spmatSRC, col); it; ++it) {
                indexType row = it.row();
                if (firstZero[row] == 0 && nextExpected[row] < col) {
                    firstZero[row] = (double)nextExpected[row] + 1;
                }
                nextExpected[row] = col + 1;
                counts[row]++;
            }
        }
        for (indexType row = 0; row < rows; ++row) {
            if (firstZero[row] == 0 && nextExpected[row] < (signedIndexType)cols) {
                firstZero[row] = (double)nextExpected[row] + 1;
            }
            if (counts[row] < cols) {
                initialized[row] = true;
                values[row] = T(0);
                indices[row] = firstZero[row] == 0 ? 1 : firstZero[row];
            }
        }
        for (signedIndexType col = 0; col < spmatSRC->outerSize(); ++col) {
            for (typename SparseMatrix::InnerIterator it(*spmatSRC, col); it; ++it) {
                indexType row = it.row();
                if (!initialized[row] || sparseMaxBetter<T>(it.value(), values[row], omitNaN)) {
                    initialized[row] = true;
                    values[row] = it.value();
                    indices[row] = (double)col + 1;
                }
            }
        }
    }

    auto* spmatDST = new SparseMatrix(outDims.rows(), outDims.columns());
    for (indexType k = 0; k < values.size(); ++k) {
        if (initialized[k] && values[k] != T(0)) {
            if (workDim == 0) {
                spmatDST->coeffRef(0, (signedIndexType)k) = values[k];
            } else {
                spmatDST->coeffRef((signedIndexType)k, 0) = values[k];
            }
        }
    }
    indexType nz = spmatDST->nonZeros();
    ArrayOf maxValue = makeSparseResult(A.dataClass(), outDims, spmatDST);
    if (nz == 0) {
        maxValue.setSparseNzmax(1);
    }
    ArrayOfVector retval;
    retval << maxValue;
    if (nLhs > 1) {
        double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, indices.size());
        for (indexType k = 0; k < indices.size(); ++k) {
            ptr[k] = indices[k];
        }
        retval << ArrayOf(NLS_DOUBLE, outDims, ptr);
    }
    return retval;
}
//=============================================================================
static ArrayOfVector
SparseMaximum(bool omitNaN, const ArrayOf& A, indexType dim, int nLhs)
{
    if (dim > 2) {
        ArrayOfVector retval;
        retval << A;
        return retval;
    }
    switch (A.dataClass()) {
    case NLS_DOUBLE:
        return SparseMaximumTemplate<double>(A, dim, nLhs, omitNaN);
    case NLS_LOGICAL:
        return SparseMaximumTemplate<logical>(A, dim, nLhs, omitNaN);
    default:
        Error(L"Nelson:data_analysis:unsupportedTypeSparseMatrix",
            L"Unsupported type in sparse matrix.");
    }
    return {};
}
//=============================================================================
// dense kernels: shared implementation in ExtremumKernels.hpp
template <class T>
void
TMaxLessInteger(
    const T* spx, const T* spy, T* dp, indexType count, indexType stridex, indexType stridey)
{
    TExtremumLessInteger<MaxPolicy, T>(spx, spy, dp, count, stridex, stridey);
}
//=============================================================================
template <class T>
void
TMaxLessReal(bool omitnan, const T* spx, const T* spy, T* dp, indexType count, indexType stridex,
    indexType stridey)
{
    TExtremumLessReal<MaxPolicy, T>(omitnan, spx, spy, dp, count, stridex, stridey);
}
//=============================================================================
template <class T>
void
TMaxLessComplex(bool omitnan, const T* spx, const T* spy, T* dp, indexType count, indexType stridex,
    indexType stridey)
{
    TExtremumLessComplex<MaxPolicy, T>(omitnan, spx, spy, dp, count, stridex, stridey);
}
//=============================================================================
template <class T>
void
TMaxAllInteger(const T* sp, T* dp, indexType elementCount)
{
    TExtremumAllInteger<MaxPolicy, T>(sp, dp, elementCount);
}
//=============================================================================
template <class T>
void
TMaxInteger(
    const T* sp, T* dp, double* iptr, indexType planes, indexType planesize, indexType linesize)
{
    TExtremumInteger<MaxPolicy, T>(sp, dp, iptr, planes, planesize, linesize);
}
//=============================================================================
template <class T>
void
TMaxAllReal(bool omitnan, const T* sp, T* dp, indexType elementCount)
{
    TExtremumAllReal<MaxPolicy, T>(omitnan, sp, dp, elementCount);
}
//=============================================================================
template <class T>
void
TMaxReal(bool omitnan, const T* sp, T* dp, double* iptr, indexType planes, indexType planesize,
    indexType linesize)
{
    TExtremumReal<MaxPolicy, T>(omitnan, sp, dp, iptr, planes, planesize, linesize);
}
//=============================================================================
template <class T>
void
TMaxAllComplex(bool omitnan, const T* sp, T* dp, indexType elementCount)
{
    TExtremumAllComplex<MaxPolicy, T>(omitnan, sp, dp, elementCount);
}
//=============================================================================
template <class T>
void
TMaxComplex(bool omitnan, const T* sp, T* dp, double* iptr, indexType planes, indexType planesize,
    indexType linesize)
{
    TExtremumComplex<MaxPolicy, T>(omitnan, sp, dp, iptr, planes, planesize, linesize);
}
//=============================================================================
template <class T>
static void
TMaxBroadcastInteger(const T* spx, const T* spy, T* dp, const BinaryBroadcast& info)
{
    TExtremumBroadcastInteger<MaxPolicy, T>(spx, spy, dp, info);
}
//=============================================================================
template <class T>
static void
TMaxBroadcastReal(bool omitnan, const T* spx, const T* spy, T* dp, const BinaryBroadcast& info)
{
    TExtremumBroadcastReal<MaxPolicy, T>(omitnan, spx, spy, dp, info);
}
//=============================================================================
template <class T>
static void
TMaxBroadcastComplex(bool omitnan, const T* spx, const T* spy, T* dp, const BinaryBroadcast& info)
{
    TExtremumBroadcastComplex<MaxPolicy, T>(omitnan, spx, spy, dp, info);
}
//=============================================================================
// C = max(A, B) with implicit expansion (A and B already share the same class)
static ArrayOf
MaximumBroadcast(bool omitNaN, const ArrayOf& A, const ArrayOf& B, const BinaryBroadcast& info,
    bool& needToOverload)
{
    needToOverload = false;
    ArrayOf res;
    NelsonType commonType = A.dataClass();
    Dimensions outDim = info.dimensions;
    switch (commonType) {
    case NLS_LOGICAL: {
        logical* ptr = (logical*)ArrayOf::allocateArrayOf(commonType, outDim.elementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMaxBroadcastInteger<logical>(
            (const logical*)A.dataPointer(), (const logical*)B.dataPointer(), ptr, info);
    } break;
    case NLS_UINT8: {
        uint8* ptr = (uint8*)ArrayOf::allocateArrayOf(commonType, outDim.elementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMaxBroadcastInteger<uint8>(
            (const uint8*)A.dataPointer(), (const uint8*)B.dataPointer(), ptr, info);
    } break;
    case NLS_INT8: {
        int8* ptr = (int8*)ArrayOf::allocateArrayOf(commonType, outDim.elementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMaxBroadcastInteger<int8>(
            (const int8*)A.dataPointer(), (const int8*)B.dataPointer(), ptr, info);
    } break;
    case NLS_UINT16: {
        uint16* ptr = (uint16*)ArrayOf::allocateArrayOf(commonType, outDim.elementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMaxBroadcastInteger<uint16>(
            (const uint16*)A.dataPointer(), (const uint16*)B.dataPointer(), ptr, info);
    } break;
    case NLS_INT16: {
        int16* ptr = (int16*)ArrayOf::allocateArrayOf(commonType, outDim.elementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMaxBroadcastInteger<int16>(
            (const int16*)A.dataPointer(), (const int16*)B.dataPointer(), ptr, info);
    } break;
    case NLS_UINT32: {
        uint32* ptr = (uint32*)ArrayOf::allocateArrayOf(commonType, outDim.elementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMaxBroadcastInteger<uint32>(
            (const uint32*)A.dataPointer(), (const uint32*)B.dataPointer(), ptr, info);
    } break;
    case NLS_INT32: {
        int32* ptr = (int32*)ArrayOf::allocateArrayOf(commonType, outDim.elementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMaxBroadcastInteger<int32>(
            (const int32*)A.dataPointer(), (const int32*)B.dataPointer(), ptr, info);
    } break;
    case NLS_UINT64: {
        uint64* ptr = (uint64*)ArrayOf::allocateArrayOf(commonType, outDim.elementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMaxBroadcastInteger<uint64>(
            (const uint64*)A.dataPointer(), (const uint64*)B.dataPointer(), ptr, info);
    } break;
    case NLS_INT64: {
        int64* ptr = (int64*)ArrayOf::allocateArrayOf(commonType, outDim.elementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMaxBroadcastInteger<int64>(
            (const int64*)A.dataPointer(), (const int64*)B.dataPointer(), ptr, info);
    } break;
    case NLS_SINGLE: {
        single* ptr = (single*)ArrayOf::allocateArrayOf(commonType, outDim.elementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMaxBroadcastReal<single>(
            omitNaN, (const single*)A.dataPointer(), (const single*)B.dataPointer(), ptr, info);
    } break;
    case NLS_DOUBLE: {
        double* ptr = (double*)ArrayOf::allocateArrayOf(commonType, outDim.elementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMaxBroadcastReal<double>(
            omitNaN, (const double*)A.dataPointer(), (const double*)B.dataPointer(), ptr, info);
    } break;
    case NLS_SCOMPLEX: {
        single* ptr = (single*)ArrayOf::allocateArrayOf(commonType, outDim.elementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMaxBroadcastComplex<single>(
            omitNaN, (const single*)A.dataPointer(), (const single*)B.dataPointer(), ptr, info);
        if (res.allReal()) {
            res.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_DCOMPLEX: {
        double* ptr = (double*)ArrayOf::allocateArrayOf(commonType, outDim.elementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMaxBroadcastComplex<double>(
            omitNaN, (const double*)A.dataPointer(), (const double*)B.dataPointer(), ptr, info);
        if (res.allReal()) {
            res.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_CHAR: {
        charType* ptr = (charType*)ArrayOf::allocateArrayOf(commonType, outDim.elementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMaxBroadcastInteger<charType>(
            (const charType*)A.dataPointer(), (const charType*)B.dataPointer(), ptr, info);
    } break;
    case NLS_HANDLE:
    case NLS_CELL_ARRAY:
    case NLS_STRUCT_ARRAY:
    case NLS_CLASS_ARRAY:
    case NLS_CLASSDEF_ARRAY:
    case NLS_FUNCTION_HANDLE:
    case NLS_STRING_ARRAY:
    default: {
        needToOverload = true;
    } break;
    }
    return res;
}
//=============================================================================
// Element-wise max of two real values with the NaN policy. For integer / logical T
// the isnan tests fold to false.
template <class T>
static T
maxCombine(T a, T b, bool omitNaN)
{
    bool na = std::isnan((double)a);
    bool nb = std::isnan((double)b);
    if (na && nb) {
        return (T)std::nan("");
    }
    if (na) {
        return omitNaN ? b : (T)std::nan("");
    }
    if (nb) {
        return omitNaN ? a : (T)std::nan("");
    }
    return (a > b) ? a : b;
}
//=============================================================================
template <class T>
static T
scalarValueAs(const ArrayOf& A)
{
    if (A.isSparse()) {
        using SparseMatrix = Eigen::SparseMatrix<T, 0, signedIndexType>;
        return static_cast<const SparseMatrix*>(A.sparseDataPointer())->coeff(0, 0);
    }
    return ((const T*)A.dataPointer())[0];
}
//=============================================================================
// Densify the sparse operand(s) and reuse the dense element-wise kernel. Used
// whenever the result is genuinely dense (e.g. max(A, 2), sparse vs full).
static ArrayOf
densifyDelegateMaximum(bool omitNaN, const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    ArrayOf Ad(A);
    ArrayOf Bd(B);
    if (Ad.isSparse()) {
        Ad.makeDense();
    }
    if (Bd.isSparse()) {
        Bd.makeDense();
    }
    return Maximum(omitNaN, Ad, Bd, needToOverload);
}
//=============================================================================
// C = max(A, B) where at least one operand is sparse and both share dataClass T.
// The result stays sparse only when it is sparsity preserving (the implicit
// zeros of the sparse operand stay zero); otherwise it densifies
// (e.g. max(A, 2) returns a full matrix).
template <class T>
static ArrayOf
SparseMaximumBinaryTemplate(bool omitNaN, const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    needToOverload = false;
    using SparseMatrix = Eigen::SparseMatrix<T, 0, signedIndexType>;

    bool aScalar = A.isScalar();
    bool bScalar = B.isScalar();
    bool bothMat = !aScalar && !bScalar;

    T fillA = aScalar ? scalarValueAs<T>(A) : T(0);
    T fillB = bScalar ? scalarValueAs<T>(B) : T(0);
    T background = maxCombine<T>(fillA, fillB, omitNaN);
    if (!sparseExtremumStaysSparse(A, B, background == T(0))) {
        return densifyDelegateMaximum(omitNaN, A, B, needToOverload);
    }
    if (A.isEmpty() || B.isEmpty()) {
        ArrayOf res = densifyDelegateMaximum(omitNaN, A, B, needToOverload);
        res.makeSparse();
        return res;
    }

    // background == 0, so only stored nonzeros can produce nonzero results.
    indexType rows = 0;
    indexType cols = 0;
    std::vector<Eigen::Triplet<T, signedIndexType>> triplets;
    if (bothMat) {
        Dimensions dims = A.dimensions();
        rows = dims.rows();
        cols = dims.columns();
        const auto* sparseA = static_cast<const SparseMatrix*>(A.sparseDataPointer());
        const auto* sparseB = static_cast<const SparseMatrix*>(B.sparseDataPointer());
        std::unordered_set<indexType> candidateIndices;
        candidateIndices.reserve(sparseA->nonZeros() + sparseB->nonZeros());
        for (signedIndexType c = 0; c < sparseA->outerSize(); ++c) {
            for (typename SparseMatrix::InnerIterator it(*sparseA, c); it; ++it) {
                candidateIndices.insert((indexType)(it.row() + it.col() * rows));
            }
        }
        for (signedIndexType c = 0; c < sparseB->outerSize(); ++c) {
            for (typename SparseMatrix::InnerIterator it(*sparseB, c); it; ++it) {
                candidateIndices.insert((indexType)(it.row() + it.col() * rows));
            }
        }
        triplets.reserve(candidateIndices.size());
        for (indexType linearIndex : candidateIndices) {
            auto r = (signedIndexType)(linearIndex % rows);
            auto c = (signedIndexType)(linearIndex / rows);
            T v = maxCombine<T>(sparseA->coeff(r, c), sparseB->coeff(r, c), omitNaN);
            if (v != T(0)) {
                triplets.emplace_back(r, c, v);
            }
        }
    } else {
        const ArrayOf& M = aScalar ? B : A;
        T fill = aScalar ? fillA : fillB;
        Dimensions dims = M.dimensions();
        rows = dims.rows();
        cols = dims.columns();
        const auto* sparseM = static_cast<const SparseMatrix*>(M.sparseDataPointer());
        triplets.reserve(sparseM->nonZeros());
        for (signedIndexType c = 0; c < sparseM->outerSize(); ++c) {
            for (typename SparseMatrix::InnerIterator it(*sparseM, c); it; ++it) {
                T v = aScalar ? maxCombine<T>(fill, it.value(), omitNaN)
                              : maxCombine<T>(it.value(), fill, omitNaN);
                if (v != T(0)) {
                    triplets.emplace_back(it.row(), it.col(), v);
                }
            }
        }
    }
    auto* spmatDST = new SparseMatrix((signedIndexType)rows, (signedIndexType)cols);
    spmatDST->setFromTriplets(triplets.begin(), triplets.end());
    indexType nz = spmatDST->nonZeros();
    ArrayOf res = makeSparseResult(A.dataClass(), Dimensions(rows, cols), spmatDST, true);
    if (nz == 0) {
        res.setSparseNzmax(1);
    }
    return res;
}
//=============================================================================
static ArrayOf
SparseMaximumBinary(bool omitNaN, const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    needToOverload = false;
    if (A.dataClass() != B.dataClass()) {
        // The builtin promotes both operands to the common type before calling,
        // so this is only a defensive path.
        return densifyDelegateMaximum(omitNaN, A, B, needToOverload);
    }
    switch (A.dataClass()) {
    case NLS_DOUBLE:
        return SparseMaximumBinaryTemplate<double>(omitNaN, A, B, needToOverload);
    case NLS_SINGLE:
        return SparseMaximumBinaryTemplate<single>(omitNaN, A, B, needToOverload);
    case NLS_LOGICAL:
        return SparseMaximumBinaryTemplate<logical>(omitNaN, A, B, needToOverload);
    case NLS_DCOMPLEX:
    case NLS_SCOMPLEX: {
        // Complex magnitude comparison is handled by the dense kernel; restore
        // sparsity afterwards with the same rule as the real classes.
        ArrayOf background
            = Maximum(omitNaN, sparseExtremumFill(A), sparseExtremumFill(B), needToOverload);
        bool keepSparse = sparseExtremumStaysSparse(A, B, sparseExtremumIsZero(background));
        ArrayOf res = densifyDelegateMaximum(omitNaN, A, B, needToOverload);
        if (!needToOverload && keepSparse) {
            res.makeSparse();
        }
        return res;
    }
    default:
        needToOverload = true;
        return {};
    }
}
//=============================================================================
// C = max(A, B)
ArrayOf
Maximum(bool omitNaN, const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    needToOverload = false;
    if (A.isClassType() || B.isClassType()) {
        needToOverload = true;
        return {};
    }
    // Empty operands go through implicit expansion too: max(zeros(0,3), 0) is 0x3.
    if (A.isSparse() || B.isSparse()) {
        return SparseMaximumBinary(omitNaN, A, B, needToOverload);
    }
    Dimensions ASize(A.dimensions());
    Dimensions BSize(B.dimensions());
    Dimensions outDim;
    ASize.simplify();
    BSize.simplify();
    indexType AStride = 1;
    indexType BStride = 1;
    if (ASize.isScalar() || BSize.isScalar() || ASize.equals(BSize)) {
        if (ASize.isScalar()) {
            outDim = BSize;
            AStride = 0;
            BStride = 1;
        } else if (BSize.isScalar()) {
            outDim = ASize;
            AStride = 1;
            BStride = 0;
        } else {
            outDim = ASize;
            AStride = 1;
            BStride = 1;
        }
    } else {
        BinaryBroadcast info = binaryOperatorBroadcast(A, B, "max");
        return MaximumBroadcast(omitNaN, A, B, info, needToOverload);
    }
    ArrayOf res;
    NelsonType commonType = A.dataClass();
    switch (A.dataClass()) {
    case NLS_LOGICAL: {
        logical* ptr = (logical*)ArrayOf::allocateArrayOf(commonType, outDim.elementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMaxLessInteger<logical>((const logical*)A.dataPointer(), (const logical*)B.dataPointer(),
            ptr, outDim.elementCount(), AStride, BStride);
    } break;
    case NLS_UINT8: {
        uint8* ptr = (uint8*)ArrayOf::allocateArrayOf(commonType, outDim.elementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMaxLessInteger<uint8>((const uint8*)A.dataPointer(), (const uint8*)B.dataPointer(), ptr,
            outDim.elementCount(), AStride, BStride);
    } break;
    case NLS_INT8: {
        int8* ptr = (int8*)ArrayOf::allocateArrayOf(commonType, outDim.elementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMaxLessInteger<int8>((const int8*)A.dataPointer(), (const int8*)B.dataPointer(), ptr,
            outDim.elementCount(), AStride, BStride);
    } break;
    case NLS_UINT16: {
        uint16* ptr = (uint16*)ArrayOf::allocateArrayOf(commonType, outDim.elementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMaxLessInteger<uint16>((const uint16*)A.dataPointer(), (const uint16*)B.dataPointer(), ptr,
            outDim.elementCount(), AStride, BStride);
    } break;
    case NLS_INT16: {
        int16* ptr = (int16*)ArrayOf::allocateArrayOf(commonType, outDim.elementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMaxLessInteger<int16>((const int16*)A.dataPointer(), (const int16*)B.dataPointer(), ptr,
            outDim.elementCount(), AStride, BStride);
    } break;
    case NLS_UINT32: {
        uint32* ptr = (uint32*)ArrayOf::allocateArrayOf(commonType, outDim.elementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMaxLessInteger<uint32>((const uint32*)A.dataPointer(), (const uint32*)B.dataPointer(), ptr,
            outDim.elementCount(), AStride, BStride);
    } break;
    case NLS_INT32: {
        int32* ptr = (int32*)ArrayOf::allocateArrayOf(commonType, outDim.elementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMaxLessInteger<int32>((const int32*)A.dataPointer(), (const int32*)B.dataPointer(), ptr,
            outDim.elementCount(), AStride, BStride);
    } break;
    case NLS_UINT64: {
        uint64* ptr = (uint64*)ArrayOf::allocateArrayOf(commonType, outDim.elementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMaxLessInteger<uint64>((const uint64*)A.dataPointer(), (const uint64*)B.dataPointer(), ptr,
            outDim.elementCount(), AStride, BStride);
    } break;
    case NLS_INT64: {
        int64* ptr = (int64*)ArrayOf::allocateArrayOf(commonType, outDim.elementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMaxLessInteger<int64>((const int64*)A.dataPointer(), (const int64*)B.dataPointer(), ptr,
            outDim.elementCount(), AStride, BStride);
    } break;
    case NLS_SINGLE: {
        single* ptr = (single*)ArrayOf::allocateArrayOf(commonType, outDim.elementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMaxLessReal<single>(omitNaN, (const single*)A.dataPointer(),
            (const single*)B.dataPointer(), ptr, outDim.elementCount(), AStride, BStride);
    } break;
    case NLS_DOUBLE: {
        double* ptr = (double*)ArrayOf::allocateArrayOf(commonType, outDim.elementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMaxLessReal<double>(omitNaN, (const double*)A.dataPointer(),
            (const double*)B.dataPointer(), ptr, outDim.elementCount(), AStride, BStride);
    } break;
    case NLS_SCOMPLEX: {
        single* ptr = (single*)ArrayOf::allocateArrayOf(commonType, outDim.elementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMaxLessComplex<single>(omitNaN, (const single*)A.dataPointer(),
            (const single*)B.dataPointer(), ptr, outDim.elementCount(), AStride, BStride);
        if (res.allReal()) {
            res.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_DCOMPLEX: {
        double* ptr = (double*)ArrayOf::allocateArrayOf(commonType, outDim.elementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMaxLessComplex<double>(omitNaN, (const double*)A.dataPointer(),
            (const double*)B.dataPointer(), ptr, outDim.elementCount(), AStride, BStride);
        if (res.allReal()) {
            res.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_CHAR: {
        charType* ptr = (charType*)ArrayOf::allocateArrayOf(commonType, outDim.elementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMaxLessInteger<charType>((const charType*)A.dataPointer(),
            (const charType*)B.dataPointer(), ptr, outDim.elementCount(), AStride, BStride);
    } break;
    case NLS_HANDLE:
    case NLS_CELL_ARRAY:
    case NLS_STRUCT_ARRAY:
    case NLS_CLASS_ARRAY:
    case NLS_CLASSDEF_ARRAY:
    case NLS_FUNCTION_HANDLE:
    case NLS_STRING_ARRAY:
    default: {
        needToOverload = true;
    } break;
    }
    return res;
}
//=============================================================================
// [M, i] = max(A)
ArrayOfVector
Maximum(bool omitNaN, const ArrayOf& A, int nLhs, bool& needToOverload)
{
    ArrayOfVector retval(1);
    needToOverload = false;
    if (A.isClassType()) {
        needToOverload = true;
        return retval;
    }
    if (A.isEmpty()) {
        retval = ExtremumOfEmpty(A, 0, nLhs);
    } else if (A.isScalar()) {
        retval << A;
        if (nLhs > 1) {
            retval << ArrayOf::doubleScalar(1);
        }
    } else {
        Dimensions dimA(A.dimensions());
        indexType d = 0;
        while (dimA[d] == 1) {
            d++;
        }
        indexType dim = d + 1;
        retval = Maximum(omitNaN, A, dim, nLhs, needToOverload);
    }
    return retval;
}
//=============================================================================
// [M, i] = max(A, [], dim)
ArrayOfVector
Maximum(bool omitNaN, const ArrayOf& A, indexType dim, int nLhs, bool& needToOverload)
{
    ArrayOfVector retval;
    needToOverload = false;
    if (dim < 1) {
        Error(L"Nelson:data_analysis:dimensionArgumentPositiveIntegerScalar",
            L"%s must be a positive integer scalar.", L"Dimension argument");
    }
    indexType workDim = dim - 1;
    if (A.isClassType()) {
        needToOverload = true;
        return retval;
    }
    if (A.isEmpty()) {
        return ExtremumOfEmpty(A, dim, nLhs);
    }
    if (A.isSparse()) {
        return SparseMaximum(omitNaN, A, dim, nLhs);
    }
    if (A.isScalar()) {
        retval << A;
        if (nLhs > 1) {
            retval << ArrayOf::doubleScalar(1);
        }
        return retval;
    }
    Dimensions dimsA = A.dimensions();
    Dimensions outDim(dimsA);
    outDim[workDim] = 1;
    indexType planecount;
    indexType planesize;
    indexType linesize = dimsA[workDim];
    planesize = 1;
    for (indexType d = 0; d < workDim; d++) {
        planesize *= dimsA[d];
    }
    planecount = 1;
    for (indexType d = workDim + 1; d < dimsA.length(); d++) {
        planecount *= dimsA[d];
    }
    outDim.simplify();
    ArrayOf index;
    double* iptr = nullptr;
    ArrayOf res;

    if (dimsA.equals(outDim)) {
        outDim.simplify();
        retval << A;
        if (nLhs > 1) {
            ompIndexType elementCount = outDim.elementCount();
            iptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, elementCount);
            index = ArrayOf(NLS_DOUBLE, outDim, iptr);
            OMP_PARALLEL_FOR_LOOP(elementCount)
            for (ompIndexType k = 0; k < (ompIndexType)elementCount; ++k) {
                iptr[k] = 1;
            }
            retval << index;
        }
        return retval;
    }

    switch (A.dataClass()) {
    case NLS_HANDLE:
    case NLS_CELL_ARRAY:
    case NLS_STRUCT_ARRAY:
    case NLS_CLASS_ARRAY:
    case NLS_CLASSDEF_ARRAY:
    case NLS_FUNCTION_HANDLE:
    case NLS_STRING_ARRAY:
    default: {
        needToOverload = true;
    } break;
    case NLS_LOGICAL: {
        if (nLhs > 1) {
            iptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.elementCount());
            index = ArrayOf(NLS_DOUBLE, outDim, iptr);
        }
        logical* ptr = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, outDim.elementCount());
        TMaxInteger<logical>(
            (const logical*)A.dataPointer(), (logical*)ptr, iptr, planecount, planesize, linesize);
        res = ArrayOf(NLS_LOGICAL, outDim, ptr);
    } break;
    case NLS_UINT8: {
        if (nLhs > 1) {
            iptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.elementCount());
            index = ArrayOf(NLS_DOUBLE, outDim, iptr);
        }
        uint8* ptr = (uint8*)ArrayOf::allocateArrayOf(NLS_UINT8, outDim.elementCount());
        TMaxInteger<uint8>(
            (const uint8*)A.dataPointer(), (uint8*)ptr, iptr, planecount, planesize, linesize);
        res = ArrayOf(NLS_UINT8, outDim, ptr);
    } break;
    case NLS_INT8: {
        if (nLhs > 1) {
            iptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.elementCount());
            index = ArrayOf(NLS_DOUBLE, outDim, iptr);
        }
        int8* ptr = (int8*)ArrayOf::allocateArrayOf(NLS_INT8, outDim.elementCount());
        TMaxInteger<int8>(
            (const int8*)A.dataPointer(), (int8*)ptr, iptr, planecount, planesize, linesize);
        res = ArrayOf(NLS_INT8, outDim, ptr);
    } break;
    case NLS_UINT16: {
        if (nLhs > 1) {
            iptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.elementCount());
            index = ArrayOf(NLS_DOUBLE, outDim, iptr);
        }
        uint16* ptr = (uint16*)ArrayOf::allocateArrayOf(NLS_UINT16, outDim.elementCount());
        TMaxInteger<uint16>(
            (const uint16*)A.dataPointer(), (uint16*)ptr, iptr, planecount, planesize, linesize);
        res = ArrayOf(NLS_UINT16, outDim, ptr);
    } break;
    case NLS_INT16: {
        if (nLhs > 1) {
            iptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.elementCount());
            index = ArrayOf(NLS_DOUBLE, outDim, iptr);
        }
        int16* ptr = (int16*)ArrayOf::allocateArrayOf(NLS_INT16, outDim.elementCount());
        TMaxInteger<int16>(
            (const int16*)A.dataPointer(), (int16*)ptr, iptr, planecount, planesize, linesize);
        res = ArrayOf(NLS_INT16, outDim, ptr);
    } break;
    case NLS_UINT32: {
        if (nLhs > 1) {
            iptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.elementCount());
            index = ArrayOf(NLS_DOUBLE, outDim, iptr);
        }
        uint32* ptr = (uint32*)ArrayOf::allocateArrayOf(NLS_UINT32, outDim.elementCount());
        TMaxInteger<uint32>(
            (const uint32*)A.dataPointer(), (uint32*)ptr, iptr, planecount, planesize, linesize);
        res = ArrayOf(NLS_UINT32, outDim, ptr);
    } break;
    case NLS_INT32: {
        if (nLhs > 1) {
            iptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.elementCount());
            index = ArrayOf(NLS_DOUBLE, outDim, iptr);
        }
        int32* ptr = (int32*)ArrayOf::allocateArrayOf(NLS_INT32, outDim.elementCount());
        TMaxInteger<int32>(
            (const int32*)A.dataPointer(), (int32*)ptr, iptr, planecount, planesize, linesize);
        res = ArrayOf(NLS_INT32, outDim, ptr);
    } break;
    case NLS_UINT64: {
        if (nLhs > 1) {
            iptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.elementCount());
            index = ArrayOf(NLS_DOUBLE, outDim, iptr);
        }
        uint64* ptr = (uint64*)ArrayOf::allocateArrayOf(NLS_UINT64, outDim.elementCount());
        TMaxInteger<uint64>(
            (const uint64*)A.dataPointer(), (uint64*)ptr, iptr, planecount, planesize, linesize);
        res = ArrayOf(NLS_UINT64, outDim, ptr);
    } break;
    case NLS_INT64: {
        if (nLhs > 1) {
            iptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.elementCount());
            index = ArrayOf(NLS_DOUBLE, outDim, iptr);
        }
        int64* ptr = (int64*)ArrayOf::allocateArrayOf(NLS_INT64, outDim.elementCount());
        TMaxInteger<int64>(
            (const int64*)A.dataPointer(), (int64*)ptr, iptr, planecount, planesize, linesize);
        res = ArrayOf(NLS_INT64, outDim, ptr);
    } break;
    case NLS_SINGLE: {
        if (nLhs > 1) {
            iptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.elementCount());
            index = ArrayOf(NLS_DOUBLE, outDim, iptr);
        }
        single* ptr = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, outDim.elementCount());
        TMaxReal<single>(omitNaN, (const single*)A.dataPointer(), (single*)ptr, iptr, planecount,
            planesize, linesize);
        res = ArrayOf(NLS_SINGLE, outDim, ptr);
    } break;
    case NLS_DOUBLE: {
        if (nLhs > 1) {
            iptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.elementCount());
            index = ArrayOf(NLS_DOUBLE, outDim, iptr);
        }
        double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.elementCount());
        TMaxReal<double>(omitNaN, (const double*)A.dataPointer(), (double*)ptr, iptr, planecount,
            planesize, linesize);
        res = ArrayOf(NLS_DOUBLE, outDim, ptr);
    } break;
    case NLS_SCOMPLEX: {
        if (nLhs > 1) {
            iptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.elementCount());
            index = ArrayOf(NLS_DOUBLE, outDim, iptr);
        }
        single* ptr = (single*)ArrayOf::allocateArrayOf(NLS_SCOMPLEX, outDim.elementCount());
        TMaxComplex<single>(omitNaN, (const single*)A.dataPointer(), (single*)ptr, iptr, planecount,
            planesize, linesize);
        res = ArrayOf(NLS_SCOMPLEX, outDim, ptr);
        if (res.allReal()) {
            res.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_DCOMPLEX: {
        if (nLhs > 1) {
            iptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.elementCount());
            index = ArrayOf(NLS_DOUBLE, outDim, iptr);
        }
        double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, outDim.elementCount());
        TMaxComplex<double>(omitNaN, (const double*)A.dataPointer(), (double*)ptr, iptr, planecount,
            planesize, linesize);
        res = ArrayOf(NLS_DCOMPLEX, outDim, ptr);
        if (res.allReal()) {
            res.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_CHAR: {
        if (nLhs > 1) {
            iptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.elementCount());
            index = ArrayOf(NLS_DOUBLE, outDim, iptr);
        }
        ArrayOf _A(A);
        _A.promoteType(NLS_DOUBLE);
        double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.elementCount());
        TMaxInteger<double>(
            (const double*)_A.dataPointer(), (double*)ptr, iptr, planecount, planesize, linesize);
        res = ArrayOf(NLS_DOUBLE, outDim, ptr);
    } break;
    }
    retval << res;
    if (nLhs > 1) {
        retval << index;
    }
    return retval;
}
//=============================================================================
ArrayOf
MaximumAll(bool omitNaN, const ArrayOf& A, bool& needToOverload)
{
    ArrayOf res;
    needToOverload = false;
    if (A.isClassType()) {
        needToOverload = true;
        return res;
    }
    if (A.isSparse()) {
        needToOverload = true;
        return res;
    }
    if (A.isEmpty()) {
        return decomplexify(A);
    }
    if (A.isScalar()) {
        return A;
    }
    Dimensions outDim(1, 1);
    indexType elementCount = A.elementCount();
    switch (A.dataClass()) {
    case NLS_HANDLE:
    case NLS_CELL_ARRAY:
    case NLS_STRUCT_ARRAY:
    case NLS_STRING_ARRAY:
    default: {
        needToOverload = true;
    } break;

    case NLS_LOGICAL: {
        logical* ptr = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, outDim.elementCount());
        TMaxAllInteger<logical>((const logical*)A.dataPointer(), (logical*)ptr, elementCount);
        res = ArrayOf(NLS_LOGICAL, outDim, ptr);
    } break;
    case NLS_UINT8: {
        uint8* ptr = (uint8*)ArrayOf::allocateArrayOf(NLS_UINT8, outDim.elementCount());
        TMaxAllInteger<uint8>((const uint8*)A.dataPointer(), (uint8*)ptr, elementCount);
        res = ArrayOf(NLS_UINT8, outDim, ptr);
    } break;
    case NLS_INT8: {
        int8* ptr = (int8*)ArrayOf::allocateArrayOf(NLS_INT8, outDim.elementCount());
        TMaxAllInteger<int8>((const int8*)A.dataPointer(), (int8*)ptr, elementCount);
        res = ArrayOf(NLS_INT8, outDim, ptr);
    } break;
    case NLS_UINT16: {
        uint16* ptr = (uint16*)ArrayOf::allocateArrayOf(NLS_UINT16, outDim.elementCount());
        TMaxAllInteger<uint16>((const uint16*)A.dataPointer(), (uint16*)ptr, elementCount);
        res = ArrayOf(NLS_UINT16, outDim, ptr);
    } break;
    case NLS_INT16: {
        int16* ptr = (int16*)ArrayOf::allocateArrayOf(NLS_INT16, outDim.elementCount());
        TMaxAllInteger<int16>((const int16*)A.dataPointer(), (int16*)ptr, elementCount);
        res = ArrayOf(NLS_INT16, outDim, ptr);
    } break;
    case NLS_UINT32: {
        uint32* ptr = (uint32*)ArrayOf::allocateArrayOf(NLS_UINT32, outDim.elementCount());
        TMaxAllInteger<uint32>((const uint32*)A.dataPointer(), (uint32*)ptr, elementCount);
        res = ArrayOf(NLS_UINT32, outDim, ptr);
    } break;
    case NLS_INT32: {
        int32* ptr = (int32*)ArrayOf::allocateArrayOf(NLS_INT32, outDim.elementCount());
        TMaxAllInteger<int32>((const int32*)A.dataPointer(), (int32*)ptr, elementCount);
        res = ArrayOf(NLS_INT32, outDim, ptr);
    } break;
    case NLS_UINT64: {
        uint64* ptr = (uint64*)ArrayOf::allocateArrayOf(NLS_UINT64, outDim.elementCount());
        TMaxAllInteger<uint64>((const uint64*)A.dataPointer(), (uint64*)ptr, elementCount);
        res = ArrayOf(NLS_UINT64, outDim, ptr);
    } break;
    case NLS_INT64: {
        int64* ptr = (int64*)ArrayOf::allocateArrayOf(NLS_INT64, outDim.elementCount());
        TMaxAllInteger<int64>((const int64*)A.dataPointer(), (int64*)ptr, elementCount);
        res = ArrayOf(NLS_INT64, outDim, ptr);
    } break;
    case NLS_SINGLE: {
        single* ptr = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, outDim.elementCount());
        TMaxAllReal<single>(omitNaN, (const single*)A.dataPointer(), (single*)ptr, elementCount);
        res = ArrayOf(NLS_SINGLE, outDim, ptr);
    } break;
    case NLS_DOUBLE: {
        double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.elementCount());
        TMaxAllReal<double>(omitNaN, (const double*)A.dataPointer(), (double*)ptr, elementCount);
        res = ArrayOf(NLS_DOUBLE, outDim, ptr);
    } break;
    case NLS_SCOMPLEX: {
        single* ptr = (single*)ArrayOf::allocateArrayOf(NLS_SCOMPLEX, outDim.elementCount());
        TMaxAllComplex<single>(omitNaN, (const single*)A.dataPointer(), (single*)ptr, elementCount);
        res = ArrayOf(NLS_SCOMPLEX, outDim, ptr);
        if (res.allReal()) {
            res.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_DCOMPLEX: {
        double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, outDim.elementCount());
        TMaxAllComplex<double>(omitNaN, (const double*)A.dataPointer(), (double*)ptr, elementCount);
        res = ArrayOf(NLS_DCOMPLEX, outDim, ptr);
        if (res.allReal()) {
            res.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_CHAR: {
        ArrayOf _A(A);
        _A.promoteType(NLS_DOUBLE);
        double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.elementCount());
        TMaxAllInteger<double>((const double*)_A.dataPointer(), (double*)ptr, elementCount);
        res = ArrayOf(NLS_DOUBLE, outDim, ptr);
    } break;
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
