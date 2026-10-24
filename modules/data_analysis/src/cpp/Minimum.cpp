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
#include "Minimum.hpp"
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
sparseMinBetter(T candidate, T current, bool omitNaN)
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
    return candidate < current;
}
//=============================================================================
template <class T>
static ArrayOfVector
SparseMinimumTemplate(const ArrayOf& A, indexType dim, int nLhs, bool omitNaN)
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
                if (!initialized[col] || sparseMinBetter<T>(it.value(), values[col], omitNaN)) {
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
                if (!initialized[row] || sparseMinBetter<T>(it.value(), values[row], omitNaN)) {
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
    ArrayOf minValue = makeSparseResult(A.dataClass(), outDims, spmatDST);
    if (nz == 0) {
        minValue.setSparseNzmax(1);
    }
    ArrayOfVector retval;
    retval << minValue;
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
SparseMinimum(bool omitNaN, const ArrayOf& A, indexType dim, int nLhs)
{
    if (dim > 2) {
        ArrayOfVector retval;
        retval << A;
        return retval;
    }
    switch (A.dataClass()) {
    case NLS_DOUBLE:
        return SparseMinimumTemplate<double>(A, dim, nLhs, omitNaN);
    case NLS_LOGICAL:
        return SparseMinimumTemplate<logical>(A, dim, nLhs, omitNaN);
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
TMinLessInteger(
    const T* spx, const T* spy, T* dp, indexType count, indexType stridex, indexType stridey)
{
    TExtremumLessInteger<MinPolicy, T>(spx, spy, dp, count, stridex, stridey);
}
//=============================================================================
template <class T>
void
TMinLessReal(bool omitnan, const T* spx, const T* spy, T* dp, indexType count, indexType stridex,
    indexType stridey)
{
    TExtremumLessReal<MinPolicy, T>(omitnan, spx, spy, dp, count, stridex, stridey);
}
//=============================================================================
template <class T>
void
TMinLessComplex(bool omitnan, const T* spx, const T* spy, T* dp, indexType count, indexType stridex,
    indexType stridey)
{
    TExtremumLessComplex<MinPolicy, T>(omitnan, spx, spy, dp, count, stridex, stridey);
}
//=============================================================================
template <class T>
void
TMinAllInteger(const T* sp, T* dp, indexType elementCount)
{
    TExtremumAllInteger<MinPolicy, T>(sp, dp, elementCount);
}
//=============================================================================
template <class T>
void
TMinInteger(
    const T* sp, T* dp, double* iptr, indexType planes, indexType planesize, indexType linesize)
{
    TExtremumInteger<MinPolicy, T>(sp, dp, iptr, planes, planesize, linesize);
}
//=============================================================================
template <class T>
void
TMinAllReal(bool omitnan, const T* sp, T* dp, indexType elementCount)
{
    TExtremumAllReal<MinPolicy, T>(omitnan, sp, dp, elementCount);
}
//=============================================================================
template <class T>
void
TMinReal(bool omitnan, const T* sp, T* dp, double* iptr, indexType planes, indexType planesize,
    indexType linesize)
{
    TExtremumReal<MinPolicy, T>(omitnan, sp, dp, iptr, planes, planesize, linesize);
}
//=============================================================================
template <class T>
void
TMinAllComplex(bool omitnan, const T* sp, T* dp, indexType elementCount)
{
    TExtremumAllComplex<MinPolicy, T>(omitnan, sp, dp, elementCount);
}
//=============================================================================
template <class T>
void
TMinComplex(bool omitnan, const T* sp, T* dp, double* iptr, indexType planes, indexType planesize,
    indexType linesize)
{
    TExtremumComplex<MinPolicy, T>(omitnan, sp, dp, iptr, planes, planesize, linesize);
}
//=============================================================================
template <class T>
static void
TMinBroadcastInteger(const T* spx, const T* spy, T* dp, const BinaryBroadcast& info)
{
    TExtremumBroadcastInteger<MinPolicy, T>(spx, spy, dp, info);
}
//=============================================================================
template <class T>
static void
TMinBroadcastReal(bool omitnan, const T* spx, const T* spy, T* dp, const BinaryBroadcast& info)
{
    TExtremumBroadcastReal<MinPolicy, T>(omitnan, spx, spy, dp, info);
}
//=============================================================================
template <class T>
static void
TMinBroadcastComplex(bool omitnan, const T* spx, const T* spy, T* dp, const BinaryBroadcast& info)
{
    TExtremumBroadcastComplex<MinPolicy, T>(omitnan, spx, spy, dp, info);
}
//=============================================================================
// C = min(A, B) with implicit expansion (A and B already share the same class)
static ArrayOf
MinimumBroadcast(bool omitNaN, const ArrayOf& A, const ArrayOf& B, const BinaryBroadcast& info,
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
        TMinBroadcastInteger<logical>(
            (const logical*)A.dataPointer(), (const logical*)B.dataPointer(), ptr, info);
    } break;
    case NLS_UINT8: {
        uint8* ptr = (uint8*)ArrayOf::allocateArrayOf(commonType, outDim.elementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMinBroadcastInteger<uint8>(
            (const uint8*)A.dataPointer(), (const uint8*)B.dataPointer(), ptr, info);
    } break;
    case NLS_INT8: {
        int8* ptr = (int8*)ArrayOf::allocateArrayOf(commonType, outDim.elementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMinBroadcastInteger<int8>(
            (const int8*)A.dataPointer(), (const int8*)B.dataPointer(), ptr, info);
    } break;
    case NLS_UINT16: {
        uint16* ptr = (uint16*)ArrayOf::allocateArrayOf(commonType, outDim.elementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMinBroadcastInteger<uint16>(
            (const uint16*)A.dataPointer(), (const uint16*)B.dataPointer(), ptr, info);
    } break;
    case NLS_INT16: {
        int16* ptr = (int16*)ArrayOf::allocateArrayOf(commonType, outDim.elementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMinBroadcastInteger<int16>(
            (const int16*)A.dataPointer(), (const int16*)B.dataPointer(), ptr, info);
    } break;
    case NLS_UINT32: {
        uint32* ptr = (uint32*)ArrayOf::allocateArrayOf(commonType, outDim.elementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMinBroadcastInteger<uint32>(
            (const uint32*)A.dataPointer(), (const uint32*)B.dataPointer(), ptr, info);
    } break;
    case NLS_INT32: {
        int32* ptr = (int32*)ArrayOf::allocateArrayOf(commonType, outDim.elementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMinBroadcastInteger<int32>(
            (const int32*)A.dataPointer(), (const int32*)B.dataPointer(), ptr, info);
    } break;
    case NLS_UINT64: {
        uint64* ptr = (uint64*)ArrayOf::allocateArrayOf(commonType, outDim.elementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMinBroadcastInteger<uint64>(
            (const uint64*)A.dataPointer(), (const uint64*)B.dataPointer(), ptr, info);
    } break;
    case NLS_INT64: {
        int64* ptr = (int64*)ArrayOf::allocateArrayOf(commonType, outDim.elementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMinBroadcastInteger<int64>(
            (const int64*)A.dataPointer(), (const int64*)B.dataPointer(), ptr, info);
    } break;
    case NLS_SINGLE: {
        single* ptr = (single*)ArrayOf::allocateArrayOf(commonType, outDim.elementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMinBroadcastReal<single>(
            omitNaN, (const single*)A.dataPointer(), (const single*)B.dataPointer(), ptr, info);
    } break;
    case NLS_DOUBLE: {
        double* ptr = (double*)ArrayOf::allocateArrayOf(commonType, outDim.elementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMinBroadcastReal<double>(
            omitNaN, (const double*)A.dataPointer(), (const double*)B.dataPointer(), ptr, info);
    } break;
    case NLS_SCOMPLEX: {
        single* ptr = (single*)ArrayOf::allocateArrayOf(commonType, outDim.elementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMinBroadcastComplex<single>(
            omitNaN, (const single*)A.dataPointer(), (const single*)B.dataPointer(), ptr, info);
        if (res.allReal()) {
            res.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_DCOMPLEX: {
        double* ptr = (double*)ArrayOf::allocateArrayOf(commonType, outDim.elementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMinBroadcastComplex<double>(
            omitNaN, (const double*)A.dataPointer(), (const double*)B.dataPointer(), ptr, info);
        if (res.allReal()) {
            res.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_CHAR: {
        charType* ptr = (charType*)ArrayOf::allocateArrayOf(commonType, outDim.elementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMinBroadcastInteger<charType>(
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
// Element-wise min of two real values with the NaN policy. For integer / logical T
// the isnan tests fold to false.
template <class T>
static T
minCombine(T a, T b, bool omitNaN)
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
    return (a < b) ? a : b;
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
// whenever the result is genuinely dense (e.g. min(A, -2), sparse vs full).
static ArrayOf
densifyDelegateMinimum(bool omitNaN, const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    ArrayOf Ad(A);
    ArrayOf Bd(B);
    if (Ad.isSparse()) {
        Ad.makeDense();
    }
    if (Bd.isSparse()) {
        Bd.makeDense();
    }
    return Minimum(omitNaN, Ad, Bd, needToOverload);
}
//=============================================================================
// C = min(A, B) where at least one operand is sparse and both share dataClass T.
// The result stays sparse only when it is sparsity preserving (the implicit
// zeros of the sparse operand stay zero); otherwise it densifies
// (e.g. min(A, -2) returns a full matrix).
template <class T>
static ArrayOf
SparseMinimumBinaryTemplate(bool omitNaN, const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    needToOverload = false;
    using SparseMatrix = Eigen::SparseMatrix<T, 0, signedIndexType>;

    bool aScalar = A.isScalar();
    bool bScalar = B.isScalar();
    bool bothMat = !aScalar && !bScalar;

    T fillA = aScalar ? scalarValueAs<T>(A) : T(0);
    T fillB = bScalar ? scalarValueAs<T>(B) : T(0);
    T background = minCombine<T>(fillA, fillB, omitNaN);
    if (!sparseExtremumStaysSparse(A, B, background == T(0))) {
        return densifyDelegateMinimum(omitNaN, A, B, needToOverload);
    }
    if (A.isEmpty() || B.isEmpty()) {
        ArrayOf res = densifyDelegateMinimum(omitNaN, A, B, needToOverload);
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
            T v = minCombine<T>(sparseA->coeff(r, c), sparseB->coeff(r, c), omitNaN);
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
                T v = aScalar ? minCombine<T>(fill, it.value(), omitNaN)
                              : minCombine<T>(it.value(), fill, omitNaN);
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
SparseMinimumBinary(bool omitNaN, const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    needToOverload = false;
    if (A.dataClass() != B.dataClass()) {
        // The builtin promotes both operands to the common type before calling,
        // so this is only a defensive path.
        return densifyDelegateMinimum(omitNaN, A, B, needToOverload);
    }
    switch (A.dataClass()) {
    case NLS_DOUBLE:
        return SparseMinimumBinaryTemplate<double>(omitNaN, A, B, needToOverload);
    case NLS_SINGLE:
        return SparseMinimumBinaryTemplate<single>(omitNaN, A, B, needToOverload);
    case NLS_LOGICAL:
        return SparseMinimumBinaryTemplate<logical>(omitNaN, A, B, needToOverload);
    case NLS_DCOMPLEX:
    case NLS_SCOMPLEX: {
        // Complex magnitude comparison is handled by the dense kernel; restore
        // sparsity afterwards with the same rule as the real classes.
        ArrayOf background
            = Minimum(omitNaN, sparseExtremumFill(A), sparseExtremumFill(B), needToOverload);
        bool keepSparse = sparseExtremumStaysSparse(A, B, sparseExtremumIsZero(background));
        ArrayOf res = densifyDelegateMinimum(omitNaN, A, B, needToOverload);
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
ArrayOf
Minimum(bool omitNaN, const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    needToOverload = false;
    if (A.isClassType() || B.isClassType()) {
        needToOverload = true;
        return {};
    }
    // Empty operands go through implicit expansion too: min(zeros(0,3), 0) is 0x3.
    if (A.isSparse() || B.isSparse()) {
        return SparseMinimumBinary(omitNaN, A, B, needToOverload);
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
        BinaryBroadcast info = binaryOperatorBroadcast(A, B, "min");
        return MinimumBroadcast(omitNaN, A, B, info, needToOverload);
    }

    ArrayOf res;
    NelsonType commonType = A.dataClass();
    switch (A.dataClass()) {
    case NLS_LOGICAL: {
        logical* ptr = (logical*)ArrayOf::allocateArrayOf(commonType, outDim.elementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMinLessInteger<logical>((const logical*)A.dataPointer(), (const logical*)B.dataPointer(),
            ptr, outDim.elementCount(), AStride, BStride);
    } break;
    case NLS_UINT8: {
        uint8* ptr = (uint8*)ArrayOf::allocateArrayOf(commonType, outDim.elementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMinLessInteger<uint8>((const uint8*)A.dataPointer(), (const uint8*)B.dataPointer(), ptr,
            outDim.elementCount(), AStride, BStride);
    } break;
    case NLS_INT8: {
        int8* ptr = (int8*)ArrayOf::allocateArrayOf(commonType, outDim.elementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMinLessInteger<int8>((const int8*)A.dataPointer(), (const int8*)B.dataPointer(), ptr,
            outDim.elementCount(), AStride, BStride);
    } break;
    case NLS_UINT16: {
        uint16* ptr = (uint16*)ArrayOf::allocateArrayOf(commonType, outDim.elementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMinLessInteger<uint16>((const uint16*)A.dataPointer(), (const uint16*)B.dataPointer(), ptr,
            outDim.elementCount(), AStride, BStride);
    } break;
    case NLS_INT16: {
        int16* ptr = (int16*)ArrayOf::allocateArrayOf(commonType, outDim.elementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMinLessInteger<int16>((const int16*)A.dataPointer(), (const int16*)B.dataPointer(), ptr,
            outDim.elementCount(), AStride, BStride);
    } break;
    case NLS_UINT32: {
        uint32* ptr = (uint32*)ArrayOf::allocateArrayOf(commonType, outDim.elementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMinLessInteger<uint32>((const uint32*)A.dataPointer(), (const uint32*)B.dataPointer(), ptr,
            outDim.elementCount(), AStride, BStride);
    } break;
    case NLS_INT32: {
        int32* ptr = (int32*)ArrayOf::allocateArrayOf(commonType, outDim.elementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMinLessInteger<int32>((const int32*)A.dataPointer(), (const int32*)B.dataPointer(), ptr,
            outDim.elementCount(), AStride, BStride);
    } break;
    case NLS_UINT64: {
        uint64* ptr = (uint64*)ArrayOf::allocateArrayOf(commonType, outDim.elementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMinLessInteger<uint64>((const uint64*)A.dataPointer(), (const uint64*)B.dataPointer(), ptr,
            outDim.elementCount(), AStride, BStride);
    } break;
    case NLS_INT64: {
        int64* ptr = (int64*)ArrayOf::allocateArrayOf(commonType, outDim.elementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMinLessInteger<int64>((const int64*)A.dataPointer(), (const int64*)B.dataPointer(), ptr,
            outDim.elementCount(), AStride, BStride);
    } break;
    case NLS_SINGLE: {
        single* ptr = (single*)ArrayOf::allocateArrayOf(commonType, outDim.elementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMinLessReal<single>(omitNaN, (const single*)A.dataPointer(),
            (const single*)B.dataPointer(), ptr, outDim.elementCount(), AStride, BStride);
    } break;
    case NLS_DOUBLE: {
        double* ptr = (double*)ArrayOf::allocateArrayOf(commonType, outDim.elementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMinLessReal<double>(omitNaN, (const double*)A.dataPointer(),
            (const double*)B.dataPointer(), ptr, outDim.elementCount(), AStride, BStride);
    } break;
    case NLS_SCOMPLEX: {
        single* ptr = (single*)ArrayOf::allocateArrayOf(commonType, outDim.elementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMinLessComplex<single>(omitNaN, (const single*)A.dataPointer(),
            (const single*)B.dataPointer(), ptr, outDim.elementCount(), AStride, BStride);
        if (res.allReal()) {
            res.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_DCOMPLEX: {
        double* ptr = (double*)ArrayOf::allocateArrayOf(commonType, outDim.elementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMinLessComplex<double>(omitNaN, (const double*)A.dataPointer(),
            (const double*)B.dataPointer(), ptr, outDim.elementCount(), AStride, BStride);
        if (res.allReal()) {
            res.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_CHAR: {
        charType* ptr = (charType*)ArrayOf::allocateArrayOf(commonType, outDim.elementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMinLessInteger<charType>((const charType*)A.dataPointer(),
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
// [M, i] = min(A)
ArrayOfVector
Minimum(bool omitNaN, const ArrayOf& A, int nLhs, bool& needToOverload)
{
    ArrayOfVector retval;
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
        retval = Minimum(omitNaN, A, dim, nLhs, needToOverload);
    }
    return retval;
}
//=============================================================================
// [M, i] = min(A, [], dim)
ArrayOfVector
Minimum(bool omitNaN, const ArrayOf& A, indexType dim, int nLhs, bool& needToOverload)
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
        return SparseMinimum(omitNaN, A, dim, nLhs);
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
            for (ompIndexType k = 0; k < elementCount; ++k) {
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
        TMinInteger<logical>(
            (const logical*)A.dataPointer(), (logical*)ptr, iptr, planecount, planesize, linesize);
        res = ArrayOf(NLS_LOGICAL, outDim, ptr);
    } break;
    case NLS_UINT8: {
        if (nLhs > 1) {
            iptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.elementCount());
            index = ArrayOf(NLS_DOUBLE, outDim, iptr);
        }
        uint8* ptr = (uint8*)ArrayOf::allocateArrayOf(NLS_UINT8, outDim.elementCount());
        TMinInteger<uint8>(
            (const uint8*)A.dataPointer(), (uint8*)ptr, iptr, planecount, planesize, linesize);
        res = ArrayOf(NLS_UINT8, outDim, ptr);
    } break;
    case NLS_INT8: {
        if (nLhs > 1) {
            iptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.elementCount());
            index = ArrayOf(NLS_DOUBLE, outDim, iptr);
        }
        int8* ptr = (int8*)ArrayOf::allocateArrayOf(NLS_INT8, outDim.elementCount());
        TMinInteger<int8>(
            (const int8*)A.dataPointer(), (int8*)ptr, iptr, planecount, planesize, linesize);
        res = ArrayOf(NLS_INT8, outDim, ptr);
    } break;
    case NLS_UINT16: {
        if (nLhs > 1) {
            iptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.elementCount());
            index = ArrayOf(NLS_DOUBLE, outDim, iptr);
        }
        uint16* ptr = (uint16*)ArrayOf::allocateArrayOf(NLS_UINT16, outDim.elementCount());
        TMinInteger<uint16>(
            (const uint16*)A.dataPointer(), (uint16*)ptr, iptr, planecount, planesize, linesize);
        res = ArrayOf(NLS_UINT16, outDim, ptr);
    } break;
    case NLS_INT16: {
        if (nLhs > 1) {
            iptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.elementCount());
            index = ArrayOf(NLS_DOUBLE, outDim, iptr);
        }
        int16* ptr = (int16*)ArrayOf::allocateArrayOf(NLS_INT16, outDim.elementCount());
        TMinInteger<int16>(
            (const int16*)A.dataPointer(), (int16*)ptr, iptr, planecount, planesize, linesize);
        res = ArrayOf(NLS_INT16, outDim, ptr);
    } break;
    case NLS_UINT32: {
        if (nLhs > 1) {
            iptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.elementCount());
            index = ArrayOf(NLS_DOUBLE, outDim, iptr);
        }
        uint32* ptr = (uint32*)ArrayOf::allocateArrayOf(NLS_UINT32, outDim.elementCount());
        TMinInteger<uint32>(
            (const uint32*)A.dataPointer(), (uint32*)ptr, iptr, planecount, planesize, linesize);
        res = ArrayOf(NLS_UINT32, outDim, ptr);
    } break;
    case NLS_INT32: {
        if (nLhs > 1) {
            iptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.elementCount());
            index = ArrayOf(NLS_DOUBLE, outDim, iptr);
        }
        int32* ptr = (int32*)ArrayOf::allocateArrayOf(NLS_INT32, outDim.elementCount());
        TMinInteger<int32>(
            (const int32*)A.dataPointer(), (int32*)ptr, iptr, planecount, planesize, linesize);
        res = ArrayOf(NLS_INT32, outDim, ptr);
    } break;
    case NLS_UINT64: {
        if (nLhs > 1) {
            iptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.elementCount());
            index = ArrayOf(NLS_DOUBLE, outDim, iptr);
        }
        uint64* ptr = (uint64*)ArrayOf::allocateArrayOf(NLS_UINT64, outDim.elementCount());
        TMinInteger<uint64>(
            (const uint64*)A.dataPointer(), (uint64*)ptr, iptr, planecount, planesize, linesize);
        res = ArrayOf(NLS_UINT64, outDim, ptr);
    } break;
    case NLS_INT64: {
        if (nLhs > 1) {
            iptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.elementCount());
            index = ArrayOf(NLS_DOUBLE, outDim, iptr);
        }
        int64* ptr = (int64*)ArrayOf::allocateArrayOf(NLS_INT64, outDim.elementCount());
        TMinInteger<int64>(
            (const int64*)A.dataPointer(), (int64*)ptr, iptr, planecount, planesize, linesize);
        res = ArrayOf(NLS_INT64, outDim, ptr);
    } break;
    case NLS_SINGLE: {
        if (nLhs > 1) {
            iptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.elementCount());
            index = ArrayOf(NLS_DOUBLE, outDim, iptr);
        }
        single* ptr = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, outDim.elementCount());
        TMinReal<single>(omitNaN, (const single*)A.dataPointer(), (single*)ptr, iptr, planecount,
            planesize, linesize);
        res = ArrayOf(NLS_SINGLE, outDim, ptr);
    } break;
    case NLS_DOUBLE: {
        if (nLhs > 1) {
            iptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.elementCount());
            index = ArrayOf(NLS_DOUBLE, outDim, iptr);
        }
        double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.elementCount());
        TMinReal<double>(omitNaN, (const double*)A.dataPointer(), (double*)ptr, iptr, planecount,
            planesize, linesize);
        res = ArrayOf(NLS_DOUBLE, outDim, ptr);
    } break;
    case NLS_SCOMPLEX: {
        if (nLhs > 1) {
            iptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.elementCount());
            index = ArrayOf(NLS_DOUBLE, outDim, iptr);
        }
        single* ptr = (single*)ArrayOf::allocateArrayOf(NLS_SCOMPLEX, outDim.elementCount());
        TMinComplex<single>(omitNaN, (const single*)A.dataPointer(), (single*)ptr, iptr, planecount,
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
        TMinComplex<double>(omitNaN, (const double*)A.dataPointer(), (double*)ptr, iptr, planecount,
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
        TMinInteger<double>(
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
MinimumAll(bool omitNaN, const ArrayOf& A, bool& needToOverload)
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
        TMinAllInteger<logical>((const logical*)A.dataPointer(), (logical*)ptr, elementCount);
        res = ArrayOf(NLS_LOGICAL, outDim, ptr);
    } break;
    case NLS_UINT8: {
        uint8* ptr = (uint8*)ArrayOf::allocateArrayOf(NLS_UINT8, outDim.elementCount());
        TMinAllInteger<uint8>((const uint8*)A.dataPointer(), (uint8*)ptr, elementCount);
        res = ArrayOf(NLS_UINT8, outDim, ptr);
    } break;
    case NLS_INT8: {
        int8* ptr = (int8*)ArrayOf::allocateArrayOf(NLS_INT8, outDim.elementCount());
        TMinAllInteger<int8>((const int8*)A.dataPointer(), (int8*)ptr, elementCount);
        res = ArrayOf(NLS_INT8, outDim, ptr);
    } break;
    case NLS_UINT16: {
        uint16* ptr = (uint16*)ArrayOf::allocateArrayOf(NLS_UINT16, outDim.elementCount());
        TMinAllInteger<uint16>((const uint16*)A.dataPointer(), (uint16*)ptr, elementCount);
        res = ArrayOf(NLS_UINT16, outDim, ptr);
    } break;
    case NLS_INT16: {
        int16* ptr = (int16*)ArrayOf::allocateArrayOf(NLS_INT16, outDim.elementCount());
        TMinAllInteger<int16>((const int16*)A.dataPointer(), (int16*)ptr, elementCount);
        res = ArrayOf(NLS_INT16, outDim, ptr);
    } break;
    case NLS_UINT32: {
        uint32* ptr = (uint32*)ArrayOf::allocateArrayOf(NLS_UINT32, outDim.elementCount());
        TMinAllInteger<uint32>((const uint32*)A.dataPointer(), (uint32*)ptr, elementCount);
        res = ArrayOf(NLS_UINT32, outDim, ptr);
    } break;
    case NLS_INT32: {
        int32* ptr = (int32*)ArrayOf::allocateArrayOf(NLS_INT32, outDim.elementCount());
        TMinAllInteger<int32>((const int32*)A.dataPointer(), (int32*)ptr, elementCount);
        res = ArrayOf(NLS_INT32, outDim, ptr);
    } break;
    case NLS_UINT64: {
        uint64* ptr = (uint64*)ArrayOf::allocateArrayOf(NLS_UINT64, outDim.elementCount());
        TMinAllInteger<uint64>((const uint64*)A.dataPointer(), (uint64*)ptr, elementCount);
        res = ArrayOf(NLS_UINT64, outDim, ptr);
    } break;
    case NLS_INT64: {
        int64* ptr = (int64*)ArrayOf::allocateArrayOf(NLS_INT64, outDim.elementCount());
        TMinAllInteger<int64>((const int64*)A.dataPointer(), (int64*)ptr, elementCount);
        res = ArrayOf(NLS_INT64, outDim, ptr);
    } break;
    case NLS_SINGLE: {
        single* ptr = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, outDim.elementCount());
        TMinAllReal<single>(omitNaN, (const single*)A.dataPointer(), (single*)ptr, elementCount);
        res = ArrayOf(NLS_SINGLE, outDim, ptr);
    } break;
    case NLS_DOUBLE: {
        double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.elementCount());
        TMinAllReal<double>(omitNaN, (const double*)A.dataPointer(), (double*)ptr, elementCount);
        res = ArrayOf(NLS_DOUBLE, outDim, ptr);
    } break;
    case NLS_SCOMPLEX: {
        single* ptr = (single*)ArrayOf::allocateArrayOf(NLS_SCOMPLEX, outDim.elementCount());
        TMinAllComplex<single>(omitNaN, (const single*)A.dataPointer(), (single*)ptr, elementCount);
        res = ArrayOf(NLS_SCOMPLEX, outDim, ptr);
        if (res.allReal()) {
            res.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_DCOMPLEX: {
        double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, outDim.elementCount());
        TMinAllComplex<double>(omitNaN, (const double*)A.dataPointer(), (double*)ptr, elementCount);
        res = ArrayOf(NLS_DCOMPLEX, outDim, ptr);
        if (res.allReal()) {
            res.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_CHAR: {
        ArrayOf _A(A);
        _A.promoteType(NLS_DOUBLE);
        double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.elementCount());
        TMinAllInteger<double>((const double*)_A.dataPointer(), (double*)ptr, elementCount);
        res = ArrayOf(NLS_DOUBLE, outDim, ptr);
    } break;
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
