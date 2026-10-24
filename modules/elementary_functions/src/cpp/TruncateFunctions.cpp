//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cmath>
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "lapack_eigen_config.hpp"
#if defined(_NLS_WITH_VML)
#include <mkl_vml.h>
#endif
#include "TruncateFunctions.hpp"
#include "ClassName.hpp"
#include "characters_encoding.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "SparseType.hpp"
#include "SparseArrayOf.hpp"
#include "i18n.hpp"
#include "Decomplexify.hpp"
#include <Eigen/Sparse>
#include <memory>
//=============================================================================
namespace Nelson {
//=============================================================================
enum TRUNCATE_LEVEL
{
    ROUND,
    CEIL,
    FIX,
    FLOOR
};
//=============================================================================
static std::wstring
getNotSupportedTypeMessage(const ArrayOf& arrayIn, TRUNCATE_LEVEL level);
//=============================================================================
template <class T>
static void
oTrunc(indexType len, const T* pIn, T* pOut)
{
    OMP_PARALLEL_FOR_LOOP(len)
    for (ompIndexType i = 0; i < (ompIndexType)(len); i++) {
        if (std::isfinite((T)pIn[i])) {
            pOut[i] = std::trunc((T)pIn[i]);
        } else {
            pOut[i] = (T)pIn[i];
        }
    }
}
//=============================================================================
template <class T>
static void
oRound(indexType len, const T* pIn, T* pOut)
{
    OMP_PARALLEL_FOR_LOOP(len)
    for (ompIndexType i = 0; i < (ompIndexType)(len); i++) {
        if (std::isfinite((T)pIn[i])) {
            pOut[i] = std::round((T)pIn[i]);
        } else {
            pOut[i] = (T)pIn[i];
        }
    }
}
//=============================================================================
template <class T>
static void
oFloor(indexType len, const T* pIn, T* pOut)
{
    OMP_PARALLEL_FOR_LOOP(len)
    for (ompIndexType i = 0; i < (ompIndexType)(len); i++) {
        if (std::isfinite((T)pIn[i])) {
            pOut[i] = std::floor((T)pIn[i]);
        } else {
            pOut[i] = (T)pIn[i];
        }
    }
}
//=============================================================================
template <class T>
static void
oCeil(indexType len, const T* pIn, T* pOut)
{
    OMP_PARALLEL_FOR_LOOP(len)
    for (ompIndexType i = 0; i < (ompIndexType)(len); i++) {
        if (std::isfinite((T)pIn[i])) {
            pOut[i] = std::ceil((T)pIn[i]);
        } else {
            pOut[i] = (T)pIn[i];
        }
    }
}
//=============================================================================
static ArrayOf
FixSingleComplex(const ArrayOf& arrayIn)
{
    indexType len = arrayIn.elementCount();
    single* ptr = (single*)ArrayOf::allocateArrayOf(NLS_SCOMPLEX, len, stringVector(), false);
    const single* dp = (const single*)arrayIn.dataPointer();
#if defined(_NLS_WITH_VML)
    vsTrunc((MKL_INT)(len * 2), dp, ptr);
#else
    oTrunc<single>(len * 2, dp, ptr);
#endif
    return ArrayOf(NLS_SCOMPLEX, arrayIn.dimensions(), ptr);
}
//=============================================================================
static ArrayOf
FixDoubleComplex(const ArrayOf& arrayIn)
{
    indexType len = arrayIn.elementCount();
    double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, len, stringVector(), false);
    const double* dp = (const double*)arrayIn.dataPointer();
#if defined(_NLS_WITH_VML)
    vdTrunc((MKL_INT)(len * 2), dp, ptr);
#else
    oTrunc<double>(len * 2, dp, ptr);
#endif
    return ArrayOf(NLS_DCOMPLEX, arrayIn.dimensions(), ptr);
}
//=============================================================================
static ArrayOf
FixSingle(const ArrayOf& arrayIn)
{
    indexType len = arrayIn.elementCount();
    single* ptr = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, len, stringVector(), false);
    const single* dp = (const single*)arrayIn.dataPointer();
#if defined(_NLS_WITH_VML)
    vsTrunc((MKL_INT)len, dp, ptr);
#else
    oTrunc<single>(len, dp, ptr);
#endif
    return ArrayOf(NLS_SINGLE, arrayIn.dimensions(), ptr);
}
//=============================================================================
static ArrayOf
FixDouble(const ArrayOf& arrayIn)
{
    indexType len = arrayIn.elementCount();
    double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, len, stringVector(), false);
    const double* dp = (const double*)arrayIn.dataPointer();
#if defined(_NLS_WITH_VML)
    vdTrunc((MKL_INT)len, dp, ptr);
#else
    oTrunc<double>(len, dp, ptr);
#endif
    return ArrayOf(NLS_DOUBLE, arrayIn.dimensions(), ptr);
}
//=============================================================================
static ArrayOf
RoundDoubleComplex(const ArrayOf& arrayIn)
{
    indexType len = arrayIn.elementCount();
    double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, len, stringVector(), false);
    const double* dp = (const double*)arrayIn.dataPointer();
#if defined(_NLS_WITH_VML)
    vdRound((MKL_INT)(len * 2), dp, ptr);
#else
    oRound<double>((len * 2), dp, ptr);
#endif
    return ArrayOf(NLS_DCOMPLEX, arrayIn.dimensions(), ptr);
}
//=============================================================================
static ArrayOf
RoundSingleComplex(const ArrayOf& arrayIn)
{
    indexType len = arrayIn.elementCount();
    single* ptr = (single*)ArrayOf::allocateArrayOf(NLS_SCOMPLEX, len, stringVector(), false);
    const single* dp = (const single*)arrayIn.dataPointer();
#if defined(_NLS_WITH_VML)
    vsRound((MKL_INT)(len * 2), dp, ptr);
#else
    oRound<single>((len * 2), dp, ptr);
#endif
    return ArrayOf(NLS_SCOMPLEX, arrayIn.dimensions(), ptr);
}
//=============================================================================
static ArrayOf
RoundDouble(const ArrayOf& arrayIn)
{
    indexType len = arrayIn.elementCount();
    double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, len, stringVector(), false);
    const double* dp = (const double*)arrayIn.dataPointer();
#if defined(_NLS_WITH_VML)
    vdRound((MKL_INT)len, dp, ptr);
#else
    oRound<double>(len, dp, ptr);
#endif
    return ArrayOf(NLS_DOUBLE, arrayIn.dimensions(), ptr);
}
//=============================================================================
static ArrayOf
RoundSingle(const ArrayOf& arrayIn)
{
    indexType len = arrayIn.elementCount();
    single* ptr = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, len, stringVector(), false);
    const single* dp = (const single*)arrayIn.dataPointer();
#if defined(_NLS_WITH_VML)
    vsRound((MKL_INT)len, dp, ptr);
#else
    oRound<single>(len, dp, ptr);
#endif
    return ArrayOf(NLS_SINGLE, arrayIn.dimensions(), ptr);
}
//=============================================================================
static ArrayOf
FloorSingleComplex(const ArrayOf& arrayIn)
{
    indexType len = arrayIn.elementCount();
    single* ptr = (single*)ArrayOf::allocateArrayOf(NLS_SCOMPLEX, len, stringVector(), false);
    const single* dp = (const single*)arrayIn.dataPointer();
#if defined(_NLS_WITH_VML)
    vsFloor((MKL_INT)(len * 2), dp, ptr);
#else
    oFloor<single>(len * 2, dp, ptr);
#endif
    return ArrayOf(NLS_SCOMPLEX, arrayIn.dimensions(), ptr);
}
//=============================================================================
static ArrayOf
FloorDoubleComplex(const ArrayOf& arrayIn)
{
    indexType len = arrayIn.elementCount();
    double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, len, stringVector(), false);
    const double* dp = (const double*)arrayIn.dataPointer();
#if defined(_NLS_WITH_VML)
    vdFloor((MKL_INT)(len * 2), dp, ptr);
#else
    oFloor<double>(len * 2, dp, ptr);
#endif
    return ArrayOf(NLS_DCOMPLEX, arrayIn.dimensions(), ptr);
}
//=============================================================================
static ArrayOf
FloorSingle(const ArrayOf& arrayIn)
{
    indexType len = arrayIn.elementCount();
    single* ptr = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, len, stringVector(), false);
    const single* dp = (const single*)arrayIn.dataPointer();
#if defined(_NLS_WITH_VML)
    vsFloor((MKL_INT)len, dp, ptr);
#else
    oFloor<single>(len, dp, ptr);
#endif
    return ArrayOf(NLS_SINGLE, arrayIn.dimensions(), ptr);
}
//=============================================================================
static ArrayOf
FloorDouble(const ArrayOf& arrayIn)
{
    indexType len = arrayIn.elementCount();
    double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, len, stringVector(), false);
    const double* dp = (const double*)arrayIn.dataPointer();
#if defined(_NLS_WITH_VML)
    vdFloor((MKL_INT)len, dp, ptr);
#else
    oFloor<double>(len, dp, ptr);
#endif
    return ArrayOf(NLS_DOUBLE, arrayIn.dimensions(), ptr);
}
//=============================================================================
static ArrayOf
CeilSingleComplex(const ArrayOf& arrayIn)
{
    indexType len = arrayIn.elementCount();
    single* ptr = (single*)ArrayOf::allocateArrayOf(NLS_SCOMPLEX, len, stringVector(), false);
    const single* dp = (const single*)arrayIn.dataPointer();
#if defined(_NLS_WITH_VML)
    vsCeil((MKL_INT)(len * 2), dp, ptr);
#else
    oCeil<single>((len * 2), dp, ptr);
#endif
    return ArrayOf(NLS_SCOMPLEX, arrayIn.dimensions(), ptr);
}
//=============================================================================
static ArrayOf
CeilDoubleComplex(const ArrayOf& arrayIn)
{
    indexType len = arrayIn.elementCount();
    double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, len, stringVector(), false);
    const double* dp = (const double*)arrayIn.dataPointer();
#if defined(_NLS_WITH_VML)
    vdCeil((MKL_INT)(len * 2), dp, ptr);
#else
    oCeil<double>((len * 2), dp, ptr);
#endif
    return ArrayOf(NLS_DCOMPLEX, arrayIn.dimensions(), ptr);
}
//=============================================================================
static ArrayOf
CeilSingle(const ArrayOf& arrayIn)
{
    indexType len = arrayIn.elementCount();
    single* ptr = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, len, stringVector(), false);
    const single* dp = (const single*)arrayIn.dataPointer();
#if defined(_NLS_WITH_VML)
    vsCeil((MKL_INT)len, dp, ptr);
#else
    oCeil<single>(len, dp, ptr);
#endif
    return ArrayOf(NLS_SINGLE, arrayIn.dimensions(), ptr);
}
//=============================================================================
static ArrayOf
CeilDouble(const ArrayOf& arrayIn)
{
    indexType len = arrayIn.elementCount();
    double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, len, stringVector(), false);
    const double* dp = (const double*)arrayIn.dataPointer();
#if defined(_NLS_WITH_VML)
    vdCeil((MKL_INT)len, dp, ptr);
#else
    oCeil<double>(len, dp, ptr);
#endif
    return ArrayOf(NLS_DOUBLE, arrayIn.dimensions(), ptr);
}
//=============================================================================
static std::wstring
getNotSupportedTypeMessage(const ArrayOf& arrayIn, TRUNCATE_LEVEL level)
{
    std::wstring name;
    ClassName(arrayIn, name);
    std::wstring functionName;
    switch (level) {
    case TRUNCATE_LEVEL::CEIL:
        functionName = L"ceil";
        break;
    case TRUNCATE_LEVEL::ROUND:
        functionName = L"round";
        break;
    case TRUNCATE_LEVEL::FIX:
        functionName = L"fix";
        break;
    case TRUNCATE_LEVEL::FLOOR:
        functionName = L"floor";
        break;
    }
    return formatI18nMessage(
        Nelson::resolveMessageTemplate(
            L"Nelson:elementary_functions:undefinedFunction", L"Undefined function '%s'"),
        name + L"_" + functionName);
}
//=============================================================================
static ArrayOf
SparseTruncate(const ArrayOf& arrayIn, TRUNCATE_LEVEL level)
{
    indexType rows = arrayIn.rows();
    indexType cols = arrayIn.columns();
    try {
        switch (arrayIn.dataClass()) {
        case NLS_LOGICAL: {
            const auto* spmatSRC
                = static_cast<const Eigen::SparseMatrix<logical, 0, signedIndexType>*>(
                    arrayIn.sparseDataPointer());
            auto spmatDST
                = std::make_unique<Eigen::SparseMatrix<double, 0, signedIndexType>>(rows, cols);
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
            const auto* spmatSRC
                = static_cast<const Eigen::SparseMatrix<double, 0, signedIndexType>*>(
                    arrayIn.sparseDataPointer());
            auto spmatDST
                = std::make_unique<Eigen::SparseMatrix<double, 0, signedIndexType>>(rows, cols);
            std::vector<Eigen::Triplet<double, signedIndexType>> triplets;
            triplets.reserve(spmatSRC->nonZeros());
            for (indexType k = 0; k < (indexType)spmatSRC->outerSize(); ++k) {
                for (Eigen::SparseMatrix<double, 0, signedIndexType>::InnerIterator it(
                         *spmatSRC, k);
                    it; ++it) {
                    double value = it.value();
                    if (std::isfinite(value)) {
                        switch (level) {
                        case TRUNCATE_LEVEL::CEIL:
                            value = std::ceil(value);
                            break;
                        case TRUNCATE_LEVEL::ROUND:
                            value = std::round(value);
                            break;
                        case TRUNCATE_LEVEL::FIX:
                            value = std::trunc(value);
                            break;
                        case TRUNCATE_LEVEL::FLOOR:
                            value = std::floor(value);
                            break;
                        }
                    }
                    if (value != 0.) {
                        triplets.emplace_back(it.row(), it.col(), value);
                    }
                }
            }
            spmatDST->setFromTriplets(triplets.begin(), triplets.end());
            return makeSparseResult(NLS_DOUBLE, arrayIn.dimensions(), std::move(spmatDST));
        } break;
        case NLS_SINGLE: {
            const auto* spmatSRC
                = static_cast<const Eigen::SparseMatrix<single, 0, signedIndexType>*>(
                    arrayIn.sparseDataPointer());
            auto spmatDST
                = std::make_unique<Eigen::SparseMatrix<single, 0, signedIndexType>>(rows, cols);
            std::vector<Eigen::Triplet<single, signedIndexType>> triplets;
            triplets.reserve(spmatSRC->nonZeros());
            for (indexType k = 0; k < (indexType)spmatSRC->outerSize(); ++k) {
                for (Eigen::SparseMatrix<single, 0, signedIndexType>::InnerIterator it(
                         *spmatSRC, k);
                    it; ++it) {
                    single value = it.value();
                    if (std::isfinite(value)) {
                        switch (level) {
                        case TRUNCATE_LEVEL::CEIL:
                            value = std::ceil(value);
                            break;
                        case TRUNCATE_LEVEL::ROUND:
                            value = std::round(value);
                            break;
                        case TRUNCATE_LEVEL::FIX:
                            value = std::trunc(value);
                            break;
                        case TRUNCATE_LEVEL::FLOOR:
                            value = std::floor(value);
                            break;
                        }
                    }
                    if (value != single(0.)) {
                        triplets.emplace_back(it.row(), it.col(), value);
                    }
                }
            }
            spmatDST->setFromTriplets(triplets.begin(), triplets.end());
            return makeSparseResult(NLS_SINGLE, arrayIn.dimensions(), std::move(spmatDST));
        } break;
        case NLS_DCOMPLEX: {
            const auto* spmatSRC
                = static_cast<const Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>*>(
                    arrayIn.sparseDataPointer());
            auto spmatDST
                = std::make_unique<Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>>(
                    rows, cols);
            std::vector<Eigen::Triplet<doublecomplex, signedIndexType>> triplets;
            triplets.reserve(spmatSRC->nonZeros());
            for (indexType k = 0; k < (indexType)spmatSRC->outerSize(); ++k) {
                for (Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>::InnerIterator it(
                         *spmatSRC, k);
                    it; ++it) {
                    double realValue = it.value().real();
                    double imagValue = it.value().imag();
                    if (std::isfinite(realValue)) {
                        switch (level) {
                        case TRUNCATE_LEVEL::CEIL:
                            realValue = std::ceil(realValue);
                            break;
                        case TRUNCATE_LEVEL::ROUND:
                            realValue = std::round(realValue);
                            break;
                        case TRUNCATE_LEVEL::FIX:
                            realValue = std::trunc(realValue);
                            break;
                        case TRUNCATE_LEVEL::FLOOR:
                            realValue = std::floor(realValue);
                            break;
                        }
                    }
                    if (std::isfinite(imagValue)) {
                        switch (level) {
                        case TRUNCATE_LEVEL::CEIL:
                            imagValue = std::ceil(imagValue);
                            break;
                        case TRUNCATE_LEVEL::ROUND:
                            imagValue = std::round(imagValue);
                            break;
                        case TRUNCATE_LEVEL::FIX:
                            imagValue = std::trunc(imagValue);
                            break;
                        case TRUNCATE_LEVEL::FLOOR:
                            imagValue = std::floor(imagValue);
                            break;
                        }
                    }
                    doublecomplex value(realValue, imagValue);
                    if (value != doublecomplex(0., 0.)) {
                        triplets.emplace_back(it.row(), it.col(), value);
                    }
                }
            }
            spmatDST->setFromTriplets(triplets.begin(), triplets.end());
            return makeSparseResult(NLS_DCOMPLEX, arrayIn.dimensions(), std::move(spmatDST));
        } break;
        case NLS_SCOMPLEX: {
            const auto* spmatSRC
                = static_cast<const Eigen::SparseMatrix<singlecomplex, 0, signedIndexType>*>(
                    arrayIn.sparseDataPointer());
            auto spmatDST
                = std::make_unique<Eigen::SparseMatrix<singlecomplex, 0, signedIndexType>>(
                    rows, cols);
            std::vector<Eigen::Triplet<singlecomplex, signedIndexType>> triplets;
            triplets.reserve(spmatSRC->nonZeros());
            for (indexType k = 0; k < (indexType)spmatSRC->outerSize(); ++k) {
                for (Eigen::SparseMatrix<singlecomplex, 0, signedIndexType>::InnerIterator it(
                         *spmatSRC, k);
                    it; ++it) {
                    single realValue = it.value().real();
                    single imagValue = it.value().imag();
                    if (std::isfinite(realValue)) {
                        switch (level) {
                        case TRUNCATE_LEVEL::CEIL:
                            realValue = std::ceil(realValue);
                            break;
                        case TRUNCATE_LEVEL::ROUND:
                            realValue = std::round(realValue);
                            break;
                        case TRUNCATE_LEVEL::FIX:
                            realValue = std::trunc(realValue);
                            break;
                        case TRUNCATE_LEVEL::FLOOR:
                            realValue = std::floor(realValue);
                            break;
                        }
                    }
                    if (std::isfinite(imagValue)) {
                        switch (level) {
                        case TRUNCATE_LEVEL::CEIL:
                            imagValue = std::ceil(imagValue);
                            break;
                        case TRUNCATE_LEVEL::ROUND:
                            imagValue = std::round(imagValue);
                            break;
                        case TRUNCATE_LEVEL::FIX:
                            imagValue = std::trunc(imagValue);
                            break;
                        case TRUNCATE_LEVEL::FLOOR:
                            imagValue = std::floor(imagValue);
                            break;
                        }
                    }
                    singlecomplex value(realValue, imagValue);
                    if (value != singlecomplex(0., 0.)) {
                        triplets.emplace_back(it.row(), it.col(), value);
                    }
                }
            }
            spmatDST->setFromTriplets(triplets.begin(), triplets.end());
            return makeSparseResult(NLS_SCOMPLEX, arrayIn.dimensions(), std::move(spmatDST));
        } break;
        default: {
        } break;
        }
    } catch (const std::bad_alloc&) {
        Error(L"Nelson:outOfMemory", L"Memory allocation error... You may have run out of memory!");
    }
    RaiseError(getNotSupportedTypeMessage(arrayIn, level), L"Nelson:truncate:typeNotSupported");
    return {};
}
//=============================================================================
static ArrayOf
Truncate(const ArrayOf& arrayIn, TRUNCATE_LEVEL level)
{
    if (arrayIn.isComplex() && arrayIn.isEmpty()) {
        // rounding no elements gives an empty real array
        return decomplexify(arrayIn);
    }
    if (arrayIn.isSparse()) {
        return SparseTruncate(arrayIn, level);
    }
    switch (arrayIn.dataClass()) {
    case NLS_SCOMPLEX: {
        switch (level) {
        case TRUNCATE_LEVEL::CEIL: {
            return CeilSingleComplex(arrayIn);
        } break;
        case TRUNCATE_LEVEL::ROUND: {
            return RoundSingleComplex(arrayIn);
        } break;
        case TRUNCATE_LEVEL::FIX: {
            return FixSingleComplex(arrayIn);
        } break;
        case TRUNCATE_LEVEL::FLOOR: {
            return FloorSingleComplex(arrayIn);
        } break;
        }
    } break;
    case NLS_DCOMPLEX: {
        switch (level) {
        case TRUNCATE_LEVEL::CEIL: {
            return CeilDoubleComplex(arrayIn);
        } break;
        case TRUNCATE_LEVEL::ROUND: {
            return RoundDoubleComplex(arrayIn);
        } break;
        case TRUNCATE_LEVEL::FIX: {
            return FixDoubleComplex(arrayIn);
        } break;
        case TRUNCATE_LEVEL::FLOOR: {
            return FloorDoubleComplex(arrayIn);
        } break;
        }
    } break;
    case NLS_LOGICAL: {
        size_t len = arrayIn.elementCount();
        void* ptr = ArrayOf::allocateArrayOf(NLS_DOUBLE, len, stringVector(), false);
        auto* rp = static_cast<double*>(ptr);
        auto* dp = (logical*)arrayIn.dataPointer();
        OMP_PARALLEL_FOR_LOOP(len)
        for (ompIndexType i = 0; i < (ompIndexType)len; i++) {
            rp[i] = (dp[i] == 0 ? 0 : 1);
        }
        return ArrayOf(NLS_DOUBLE, arrayIn.dimensions(), rp);
    } break;
    case NLS_SINGLE: {
        switch (level) {
        case TRUNCATE_LEVEL::CEIL: {
            return CeilSingle(arrayIn);
        } break;
        case TRUNCATE_LEVEL::ROUND: {
            return RoundSingle(arrayIn);
        } break;
        case TRUNCATE_LEVEL::FIX: {
            return FixSingle(arrayIn);
        } break;
        case TRUNCATE_LEVEL::FLOOR: {
            return FloorSingle(arrayIn);
        } break;
        }
    } break;
    case NLS_DOUBLE: {
        switch (level) {
        case TRUNCATE_LEVEL::CEIL: {
            return CeilDouble(arrayIn);
        } break;
        case TRUNCATE_LEVEL::ROUND: {
            return RoundDouble(arrayIn);
        } break;
        case TRUNCATE_LEVEL::FIX: {
            return FixDouble(arrayIn);
        } break;
        case TRUNCATE_LEVEL::FLOOR: {
            return FloorDouble(arrayIn);
        } break;
        }
    }
    case NLS_UINT8:
    case NLS_INT8:
    case NLS_UINT16:
    case NLS_INT16:
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_UINT64:
    case NLS_INT64: {
        return arrayIn;
    } break;
    case NLS_CHAR: {
        size_t len = arrayIn.elementCount();
        void* ptr = ArrayOf::allocateArrayOf(NLS_DOUBLE, len, stringVector(), false);
        auto* rp = static_cast<double*>(ptr);
        auto* dp = (charType*)arrayIn.dataPointer();
        OMP_PARALLEL_FOR_LOOP(len)
        for (ompIndexType i = 0; i < (ompIndexType)len; i++) {
            rp[i] = static_cast<double>(dp[i]);
        }
        return ArrayOf(NLS_DOUBLE, arrayIn.dimensions(), rp);
    } break;
    case NLS_HANDLE:
    case NLS_GO_HANDLE:
    case NLS_CELL_ARRAY:
    case NLS_STRING_ARRAY:
    case NLS_STRUCT_ARRAY:
    case NLS_CLASS_ARRAY:
    case NLS_CLASSDEF_ARRAY:
    case NLS_FUNCTION_HANDLE:
    default: {
        RaiseError(getNotSupportedTypeMessage(arrayIn, level), L"Nelson:truncate:typeNotSupported");
    } break;
    }
    return {};
}
//=============================================================================
ArrayOf
Round(const ArrayOf& arrayIn)
{
    return Truncate(arrayIn, TRUNCATE_LEVEL::ROUND);
}
//=============================================================================
ArrayOf
Ceil(const ArrayOf& arrayIn)
{
    return Truncate(arrayIn, TRUNCATE_LEVEL::CEIL);
}
//=============================================================================
ArrayOf
Floor(const ArrayOf& arrayIn)
{
    return Truncate(arrayIn, TRUNCATE_LEVEL::FLOOR);
}
//=============================================================================
ArrayOf
Fix(const ArrayOf& arrayIn)
{
    return Truncate(arrayIn, TRUNCATE_LEVEL::FIX);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
