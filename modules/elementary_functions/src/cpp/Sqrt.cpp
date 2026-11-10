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
#include <limits>
#include <cmath>
#include <complex>
#include <memory>
#include <vector>
#include <Eigen/Dense>
#include <Eigen/Sparse>
#include "Sqrt.hpp"
#include "SparseType.hpp"
#include "SparseArrayOf.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Real value as a complex square root: sqrt(-0) is +0, negatives are pure imaginary.
template <class T>
static inline std::complex<T>
SqrtRealAsComplex(T x)
{
    if (x < 0) {
        return std::complex<T>((T)0, std::sqrt(-x));
    }
    return std::complex<T>(x == 0 ? (T)0 : std::sqrt(x), (T)0);
}
//=============================================================================
// A zero imaginary part (of either sign) is read as a real value; other NaN give NaN + NaNi.
template <class T>
static inline std::complex<T>
SqrtComplexScalar(std::complex<T> z)
{
    if (z.imag() == 0) {
        return SqrtRealAsComplex<T>(z.real());
    }
    if (std::isnan(z.real()) || std::isnan(z.imag())) {
        T nan = std::numeric_limits<T>::quiet_NaN();
        return std::complex<T>(nan, nan);
    }
    return std::sqrt(z);
}
//=============================================================================
// sqrt(-0) is +0.
template <class T>
static inline T
SqrtRealScalar(T value)
{
    return value == 0 ? (T)0 : std::sqrt(value);
}
//=============================================================================
template <class T>
static bool
haveNegativeValue(T* values, indexType lengthValues)
{
    for (indexType k = 0; k < lengthValues; k++) {
        if (values[k] < 0) {
            return true;
        }
    }
    return false;
}
//=============================================================================
template <class T>
static ArrayOf
SqrtReal(NelsonType classDestination, const ArrayOf& A)
{
    Dimensions dimsA = A.dimensions();
    ompIndexType elementCount = (ompIndexType)A.elementCount();
    T* ptrIn = (T*)A.dataPointer();
    T* ptrOut = (T*)ArrayOf::allocateArrayOf(classDestination, elementCount, stringVector(), false);
#if defined(_NLS_WITH_VML)
    Eigen::Map<Eigen::Matrix<T, 1, Eigen::Dynamic>> matIn(ptrIn, elementCount);
    Eigen::Map<Eigen::Matrix<T, 1, Eigen::Dynamic>> matOut(ptrOut, elementCount);
    matOut = (matIn.array() == (T)0).select((T)0, matIn.array().sqrt());
#else
    OMP_PARALLEL_FOR_LOOP(elementCount)
    for (ompIndexType k = 0; k < elementCount; k++) {
        ptrOut[k] = SqrtRealScalar<T>(ptrIn[k]);
    }
#endif
    return ArrayOf(classDestination, dimsA, ptrOut);
}
//=============================================================================
template <class T>
static ArrayOf
SqrtComplex(NelsonType classDestination, const ArrayOf& A)
{
    Dimensions dimsA = A.dimensions();
    ompIndexType elementCount = (ompIndexType)A.elementCount();
    T* ptrOut = (T*)ArrayOf::allocateArrayOf(
        classDestination, dimsA.elementCount(), stringVector(), false);
    std::complex<T>* Cz = reinterpret_cast<std::complex<T>*>((T*)ptrOut);
    const std::complex<T>* Az = reinterpret_cast<const std::complex<T>*>(A.dataPointer());
    OMP_PARALLEL_FOR_LOOP(elementCount)
    for (ompIndexType k = 0; k < elementCount; k++) {
        Cz[k] = SqrtComplexScalar<T>(Az[k]);
    }
    return ArrayOf(classDestination, dimsA, ptrOut);
}
//=============================================================================
template <class T>
static ArrayOf
SqrtRealToComplex(NelsonType classDestination, const ArrayOf& A)
{
    Dimensions dimsA = A.dimensions();
    ompIndexType elementCount = (ompIndexType)A.elementCount();
    T* ptrOut = (T*)ArrayOf::allocateArrayOf(
        classDestination, dimsA.elementCount(), stringVector(), false);
    std::complex<T>* Cz = reinterpret_cast<std::complex<T>*>((T*)ptrOut);
    const T* ptrIn = (const T*)A.dataPointer();
    OMP_PARALLEL_FOR_LOOP(elementCount)
    for (ompIndexType k = 0; k < elementCount; k++) {
        Cz[k] = SqrtRealAsComplex<T>(ptrIn[k]);
    }
    return ArrayOf(classDestination, dimsA, ptrOut);
}
//=============================================================================
template <class T>
static ArrayOf
SparseSqrtReal(const ArrayOf& A, NelsonType realClass, NelsonType complexClass)
{
    // sqrt preserves zeros, so only the stored nonzeros need to be visited:
    // O(nnz) instead of densifying to a full rows*cols buffer.
    indexType rows = A.rows();
    indexType cols = A.columns();
    const auto* spmatSRC
        = static_cast<const Eigen::SparseMatrix<T, 0, signedIndexType>*>(A.sparseDataPointer());
    bool anyNegative = false;
    for (signedIndexType k = 0; k < spmatSRC->outerSize() && !anyNegative; ++k) {
        for (typename Eigen::SparseMatrix<T, 0, signedIndexType>::InnerIterator it(*spmatSRC, k);
            it; ++it) {
            if (it.value() < T(0)) {
                anyNegative = true;
                break;
            }
        }
    }
    if (!anyNegative) {
        // sqrt of a nonnegative entry stays nonnegative, so the sparsity pattern
        // is unchanged: copy it and transform the stored values with the same
        // routine the dense path uses, for bit-for-bit parity with full sqrt.
        auto spmatDST = std::make_unique<Eigen::SparseMatrix<T, 0, signedIndexType>>(*spmatSRC);
        spmatDST->makeCompressed();
        T* values = spmatDST->valuePtr();
        ompIndexType nnz = (ompIndexType)spmatDST->nonZeros();
#if defined(_NLS_WITH_VML)
        Eigen::Map<Eigen::Matrix<T, 1, Eigen::Dynamic>> mapValues(values, nnz);
        mapValues = mapValues.array().sqrt();
#else
        OMP_PARALLEL_FOR_LOOP(nnz)
        for (ompIndexType i = 0; i < nnz; ++i) {
            values[i] = SqrtRealScalar<T>(values[i]);
        }
#endif
        return makeSparseResult(realClass, A.dimensions(), std::move(spmatDST));
    }
    // A negative real entry maps to a pure imaginary value, so the result must
    // become a complex sparse matrix.
    auto spmatDST
        = std::make_unique<Eigen::SparseMatrix<std::complex<T>, 0, signedIndexType>>(rows, cols);
    std::vector<Eigen::Triplet<std::complex<T>, signedIndexType>> triplets;
    triplets.reserve(spmatSRC->nonZeros());
    bool allReal = true;
    for (signedIndexType k = 0; k < spmatSRC->outerSize(); ++k) {
        for (typename Eigen::SparseMatrix<T, 0, signedIndexType>::InnerIterator it(*spmatSRC, k);
            it; ++it) {
            std::complex<T> value = SqrtRealAsComplex<T>(it.value());
            if (value.imag() != 0) {
                allReal = false;
            }
            if (value != std::complex<T>(0)) {
                triplets.emplace_back(it.row(), it.col(), value);
            }
        }
    }
    spmatDST->setFromTriplets(triplets.begin(), triplets.end());
    ArrayOf res = makeSparseResult(complexClass, A.dimensions(), std::move(spmatDST));
    if (allReal) {
        res.promoteType(realClass);
    }
    return res;
}
//=============================================================================
ArrayOf
Sqrt(const ArrayOf& A, bool& needToOverload)
{
    ArrayOf res;
    needToOverload = false;
    if (A.isSparse()) {
        switch (A.dataClass()) {
        case NLS_DOUBLE:
            return SparseSqrtReal<double>(A, NLS_DOUBLE, NLS_DCOMPLEX);
        case NLS_SINGLE:
            return SparseSqrtReal<single>(A, NLS_SINGLE, NLS_SCOMPLEX);
        default: {
            // complex / logical sparse: sqrt still preserves zeros, so densify,
            // compute, and re-sparsify.
            ArrayOf dense(A);
            dense.makeDense();
            res = Sqrt(dense, needToOverload);
            if (!needToOverload) {
                res.makeSparse();
            }
            return res;
        }
        }
    }
    switch (A.dataClass()) {
    case NLS_DOUBLE: {
        if (haveNegativeValue<double>((double*)A.dataPointer(), A.elementCount())) {
            return SqrtRealToComplex<double>(NLS_DCOMPLEX, A);
        }
        return SqrtReal<double>(NLS_DOUBLE, A);
    } break;
    case NLS_SINGLE: {
        if (haveNegativeValue<single>((single*)A.dataPointer(), A.elementCount())) {
            return SqrtRealToComplex<single>(NLS_SCOMPLEX, A);
        }
        return SqrtReal<single>(NLS_SINGLE, A);
    } break;
    case NLS_DCOMPLEX: {
        res = SqrtComplex<double>(NLS_DCOMPLEX, A);
        if (res.allReal()) {
            res.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_SCOMPLEX: {
        res = SqrtComplex<single>(NLS_SCOMPLEX, A);
        if (res.allReal()) {
            res.promoteType(NLS_SINGLE);
        }
    } break;
    default: {
        needToOverload = true;
        return res;
    } break;
    }
    return res;
    //=============================================================================
} // namespace Nelson
//=============================================================================
} // namespace Nelson
//=============================================================================
