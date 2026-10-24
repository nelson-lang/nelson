//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "lapack_eigen_config.hpp"
#if defined(_NLS_WITH_VML)
#include <mkl_vml.h>
#endif
#include "AbsoluteValue.hpp"
#include "ClassName.hpp"
#include "characters_encoding.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "SparseType.hpp"
#include "SparseArrayOf.hpp"
#include <Eigen/Sparse>
#include <limits>
#include <memory>
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
static inline T
saturatedAbsoluteValue(T v)
{
    if (std::numeric_limits<T>::is_integer && std::numeric_limits<T>::is_signed
        && v == std::numeric_limits<T>::min()) {
        // abs(intmin) saturates to intmax.
        return std::numeric_limits<T>::max();
    }
    return static_cast<T>(std::abs(v));
}
//=============================================================================
template <class InputType, class OutputType>
static void
fillSparseAbsoluteValue(
    const ArrayOf& arrayIn, Eigen::SparseMatrix<OutputType, 0, signedIndexType>* spmatDST)
{
    const auto* spmatSRC = static_cast<const Eigen::SparseMatrix<InputType, 0, signedIndexType>*>(
        arrayIn.sparseDataPointer());
    std::vector<Eigen::Triplet<OutputType, signedIndexType>> triplets;
    triplets.reserve(spmatSRC->nonZeros());
    for (indexType k = 0; k < (indexType)spmatSRC->outerSize(); ++k) {
        for (typename Eigen::SparseMatrix<InputType, 0, signedIndexType>::InnerIterator it(
                 *spmatSRC, k);
            it; ++it) {
            OutputType value = static_cast<OutputType>(std::abs(it.value()));
            if (value != OutputType(0)) {
                triplets.emplace_back(it.row(), it.col(), value);
            }
        }
    }
    spmatDST->setFromTriplets(triplets.begin(), triplets.end());
}
//=============================================================================
static ArrayOf
SparseAbsoluteValue(const ArrayOf& arrayIn)
{
    indexType rows = arrayIn.rows();
    indexType cols = arrayIn.columns();
    try {
        switch (arrayIn.dataClass()) {
        case NLS_LOGICAL: {
            auto spmatDST
                = std::make_unique<Eigen::SparseMatrix<double, 0, signedIndexType>>(rows, cols);
            const auto* spmatSRC
                = static_cast<const Eigen::SparseMatrix<logical, 0, signedIndexType>*>(
                    arrayIn.sparseDataPointer());
            std::vector<Eigen::Triplet<double, signedIndexType>> triplets;
            triplets.reserve(spmatSRC->nonZeros());
            for (indexType k = 0; k < (indexType)spmatSRC->outerSize(); ++k) {
                for (Eigen::SparseMatrix<logical, 0, signedIndexType>::InnerIterator it(
                         *spmatSRC, k);
                    it; ++it) {
                    if (it.value()) {
                        triplets.emplace_back(it.row(), it.col(), 1.);
                    }
                }
            }
            spmatDST->setFromTriplets(triplets.begin(), triplets.end());
            return makeSparseResult(NLS_DOUBLE, arrayIn.dimensions(), std::move(spmatDST));
        } break;
        case NLS_DOUBLE: {
            auto spmatDST
                = std::make_unique<Eigen::SparseMatrix<double, 0, signedIndexType>>(rows, cols);
            fillSparseAbsoluteValue<double, double>(arrayIn, spmatDST.get());
            return makeSparseResult(NLS_DOUBLE, arrayIn.dimensions(), std::move(spmatDST));
        } break;
        case NLS_DCOMPLEX: {
            auto spmatDST
                = std::make_unique<Eigen::SparseMatrix<double, 0, signedIndexType>>(rows, cols);
            fillSparseAbsoluteValue<doublecomplex, double>(arrayIn, spmatDST.get());
            return makeSparseResult(NLS_DOUBLE, arrayIn.dimensions(), std::move(spmatDST));
        } break;
        case NLS_SINGLE: {
            auto spmatDST
                = std::make_unique<Eigen::SparseMatrix<single, 0, signedIndexType>>(rows, cols);
            fillSparseAbsoluteValue<single, single>(arrayIn, spmatDST.get());
            return makeSparseResult(NLS_SINGLE, arrayIn.dimensions(), std::move(spmatDST));
        } break;
        case NLS_SCOMPLEX: {
            auto spmatDST
                = std::make_unique<Eigen::SparseMatrix<single, 0, signedIndexType>>(rows, cols);
            fillSparseAbsoluteValue<singlecomplex, single>(arrayIn, spmatDST.get());
            return makeSparseResult(NLS_SINGLE, arrayIn.dimensions(), std::move(spmatDST));
        } break;
        default: {
            return {};
        } break;
        }
    } catch (const std::bad_alloc&) {
        Error(L"Nelson:outOfMemory", L"Memory allocation error... You may have run out of memory!");
    }
    return {};
}
//=============================================================================
template <class T>
void
absoluteValueRealTemplate(T* ptrA, indexType N, T* ptrRes)
{
    OMP_PARALLEL_FOR_LOOP(N)
    for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
        ptrRes[i] = saturatedAbsoluteValue<T>(ptrA[i]);
    }
}
//=============================================================================
template <class T>
void
absoluteValueComplexTemplate(T* ptrA, indexType N, T* ptrRes)
{
    auto* matCplxA = reinterpret_cast<std::complex<T>*>(ptrA);
    OMP_PARALLEL_FOR_LOOP(N)
    for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
        ptrRes[i] = std::abs(matCplxA[i]);
    }
}
//=============================================================================
ArrayOf
AbsoluteValue(const ArrayOf& arrayIn, bool& needToOverload)
{
    ArrayOf res;
    needToOverload = false;
    if (arrayIn.isSparse()) {
        res = SparseAbsoluteValue(arrayIn);
        if (res.isEmpty() && !arrayIn.isEmpty()) {
            needToOverload = true;
        }
        return res;
    }
    switch (arrayIn.dataClass()) {
    case NLS_HANDLE:
    case NLS_GO_HANDLE:
    case NLS_CELL_ARRAY:
    case NLS_STRING_ARRAY:
    case NLS_STRUCT_ARRAY:
    case NLS_CLASS_ARRAY:
    case NLS_CLASSDEF_ARRAY:
    case NLS_FUNCTION_HANDLE:
    default: {
        needToOverload = true;
        return {};
    } break;
    case NLS_UINT8:
    case NLS_UINT16:
    case NLS_UINT32:
    case NLS_UINT64: {
        res = arrayIn;
        res.ensureSingleOwner();
    } break;
    case NLS_LOGICAL:
    case NLS_CHAR: {
        res = arrayIn;
        res.ensureSingleOwner();
        res.promoteType(NLS_DOUBLE);
    } break;
    case NLS_INT8: {
        if (arrayIn.isScalar()) {
            return ArrayOf::int8Scalar(
                saturatedAbsoluteValue<int8>(((int8*)arrayIn.dataPointer())[0]));
        }
        Dimensions dimsRes = arrayIn.dimensions();
        int8* ptrRes = (int8*)ArrayOf::allocateArrayOf(arrayIn.dataClass(), dimsRes.elementCount());
        res = ArrayOf(arrayIn.dataClass(), dimsRes, ptrRes);
        absoluteValueRealTemplate<int8>(
            (int8*)arrayIn.dataPointer(), dimsRes.elementCount(), ptrRes);
    } break;
    case NLS_INT16: {
        if (arrayIn.isScalar()) {
            return ArrayOf::int16Scalar(
                saturatedAbsoluteValue<int16>(((int16*)arrayIn.dataPointer())[0]));
        }
        Dimensions dimsRes = arrayIn.dimensions();
        int16* ptrRes
            = (int16*)ArrayOf::allocateArrayOf(arrayIn.dataClass(), dimsRes.elementCount());
        res = ArrayOf(arrayIn.dataClass(), dimsRes, ptrRes);
        absoluteValueRealTemplate<int16>(
            (int16*)arrayIn.dataPointer(), dimsRes.elementCount(), ptrRes);
    } break;
    case NLS_INT32: {
        if (arrayIn.isScalar()) {
            return ArrayOf::int32Scalar(
                saturatedAbsoluteValue<int32>(((int32*)arrayIn.dataPointer())[0]));
        }
        Dimensions dimsRes = arrayIn.dimensions();
        int32* ptrRes
            = (int32*)ArrayOf::allocateArrayOf(arrayIn.dataClass(), dimsRes.elementCount());
        res = ArrayOf(arrayIn.dataClass(), dimsRes, ptrRes);
        absoluteValueRealTemplate<int32>(
            (int32*)arrayIn.dataPointer(), dimsRes.elementCount(), ptrRes);
    } break;
    case NLS_INT64: {
        if (arrayIn.isScalar()) {
            return ArrayOf::int64Scalar(
                saturatedAbsoluteValue<int64>(((int64*)arrayIn.dataPointer())[0]));
        }
        Dimensions dimsRes = arrayIn.dimensions();
        int64* ptrRes
            = (int64*)ArrayOf::allocateArrayOf(arrayIn.dataClass(), dimsRes.elementCount());
        res = ArrayOf(arrayIn.dataClass(), dimsRes, ptrRes);
        absoluteValueRealTemplate<int64>(
            (int64*)arrayIn.dataPointer(), dimsRes.elementCount(), ptrRes);
    } break;
    case NLS_SINGLE: {
        if (arrayIn.isScalar()) {
            return ArrayOf::singleScalar(std::abs(((single*)arrayIn.dataPointer())[0]));
        }
        Dimensions dimsRes = arrayIn.dimensions();
        single* ptrRes
            = (single*)ArrayOf::allocateArrayOf(arrayIn.dataClass(), dimsRes.elementCount());
        res = ArrayOf(arrayIn.dataClass(), dimsRes, ptrRes);
#if defined(_NLS_WITH_VML)
        vsAbs((MKL_INT)arrayIn.elementCount(), (const single*)arrayIn.dataPointer(), ptrRes);
#else
        absoluteValueRealTemplate<single>(
            (single*)arrayIn.dataPointer(), dimsRes.elementCount(), ptrRes);
#endif
    } break;
    case NLS_DOUBLE: {
        if (arrayIn.isScalar()) {
            return ArrayOf::doubleScalar(std::abs(((double*)arrayIn.dataPointer())[0]));
        }
        Dimensions dimsRes = arrayIn.dimensions();
        double* ptrRes
            = (double*)ArrayOf::allocateArrayOf(arrayIn.dataClass(), dimsRes.elementCount());
        res = ArrayOf(arrayIn.dataClass(), dimsRes, ptrRes);
#if defined(_NLS_WITH_VML)
        vdAbs((MKL_INT)arrayIn.elementCount(), (const double*)arrayIn.dataPointer(), ptrRes);
#else
        absoluteValueRealTemplate<double>(
            (double*)arrayIn.dataPointer(), dimsRes.elementCount(), ptrRes);
#endif
    } break;
    case NLS_DCOMPLEX: {
        Dimensions dimsRes = arrayIn.dimensions();
        double* ptrRes = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, dimsRes.elementCount());
        res = ArrayOf(NLS_DOUBLE, dimsRes, ptrRes);
#if defined(_NLS_WITH_VML)
        MKL_Complex16* ptrAz = reinterpret_cast<MKL_Complex16*>((double*)arrayIn.dataPointer());
        vzAbs((MKL_INT)arrayIn.elementCount(), ptrAz, ptrRes);
#else
        absoluteValueComplexTemplate<double>(
            (double*)arrayIn.dataPointer(), dimsRes.elementCount(), ptrRes);
#endif
    } break;
    case NLS_SCOMPLEX: {
        Dimensions dimsRes = arrayIn.dimensions();
        single* ptrRes = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, dimsRes.elementCount());
        res = ArrayOf(NLS_SINGLE, dimsRes, ptrRes);
#if defined(_NLS_WITH_VML)
        MKL_Complex8* ptrAz = reinterpret_cast<MKL_Complex8*>((single*)arrayIn.dataPointer());
        vcAbs((MKL_INT)arrayIn.elementCount(), ptrAz, ptrRes);
#else
        absoluteValueComplexTemplate<single>(
            (single*)arrayIn.dataPointer(), arrayIn.elementCount(), ptrRes);
#endif
    } break;
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
