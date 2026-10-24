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
#include <complex>
#include <Eigen/Dense>
#include <Eigen/Sparse>
#include <memory>
#include "Exponential.hpp"
#include "SparseType.hpp"
#include "SparseArrayOf.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
T
ExponentialComplexScalar(T value)
{
    T res(exp(value.real()) * cos(value.imag()), exp(value.real()) * sin(value.imag()));
    return res;
}
//=============================================================================
template <class T>
T
ExponentialRealScalar(T value)
{
    return exp(value);
}
//=============================================================================
static ArrayOf
SparseExponential(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    indexType rows = A.rows();
    indexType cols = A.columns();
    switch (A.dataClass()) {
    case NLS_DOUBLE: {
        auto spmatDST
            = std::make_unique<Eigen::SparseMatrix<double, 0, signedIndexType>>(rows, cols);
        for (indexType col = 0; col < cols; ++col) {
            for (indexType row = 0; row < rows; ++row) {
                spmatDST->coeffRef(row, col) = 1.;
            }
        }
        const auto* spmatSRC = static_cast<const Eigen::SparseMatrix<double, 0, signedIndexType>*>(
            A.sparseDataPointer());
        for (indexType k = 0; k < (indexType)spmatSRC->outerSize(); ++k) {
            for (Eigen::SparseMatrix<double, 0, signedIndexType>::InnerIterator it(*spmatSRC, k);
                it; ++it) {
                spmatDST->coeffRef(it.row(), it.col()) = exp(it.value());
            }
        }
        return makeSparseResult(NLS_DOUBLE, A.dimensions(), std::move(spmatDST));
    } break;
    case NLS_DCOMPLEX: {
        auto spmatDST
            = std::make_unique<Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>>(rows, cols);
        for (indexType col = 0; col < cols; ++col) {
            for (indexType row = 0; row < rows; ++row) {
                spmatDST->coeffRef(row, col) = doublecomplex(1., 0.);
            }
        }
        const auto* spmatSRC
            = static_cast<const Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>*>(
                A.sparseDataPointer());
        for (indexType k = 0; k < (indexType)spmatSRC->outerSize(); ++k) {
            for (Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>::InnerIterator it(
                     *spmatSRC, k);
                it; ++it) {
                spmatDST->coeffRef(it.row(), it.col())
                    = ExponentialComplexScalar<doublecomplex>(it.value());
            }
        }
        return makeSparseResult(NLS_DCOMPLEX, A.dimensions(), std::move(spmatDST));
    } break;
    default: {
        needToOverload = true;
        return {};
    } break;
    }
}
//=============================================================================
ArrayOf
Exponential(const ArrayOf& A, bool& needToOverload)
{
    ArrayOf res;
    needToOverload = false;
    NelsonType classA = A.dataClass();
    if (A.isSparse()) {
        return SparseExponential(A, needToOverload);
    }
    if (classA == NLS_DOUBLE || classA == NLS_DCOMPLEX) {
        Dimensions dimsA = A.dimensions();
        indexType nElementsA = A.elementCount();
        if (classA == NLS_DOUBLE) {
            double* ptrOut
                = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, nElementsA, stringVector(), false);
            auto* ptrIn = (double*)A.dataPointer();
            Eigen::Map<Eigen::ArrayXd> matOut(ptrOut, nElementsA);
            Eigen::Map<Eigen::ArrayXd> matIn(ptrIn, nElementsA);
            matOut = matIn.array().exp();
            res = ArrayOf(NLS_DOUBLE, dimsA, ptrOut);
        } else {
            double* ptrOut = (double*)ArrayOf::allocateArrayOf(
                NLS_DCOMPLEX, nElementsA, stringVector(), false);
            auto* Cz = reinterpret_cast<std::complex<double>*>(ptrOut);
            double* ptrIn = (double*)A.dataPointer();
            auto* Az = reinterpret_cast<std::complex<double>*>(ptrIn);
            Eigen::Map<Eigen::ArrayXcd> matOut(Cz, nElementsA);
            Eigen::Map<Eigen::ArrayXcd> matIn(Az, nElementsA);
#if defined(_NLS_WITH_VML)
            matOut = matIn.array().exp();
#else
            ompIndexType elementCount = (ompIndexType)nElementsA;
            OMP_PARALLEL_FOR_LOOP(elementCount)
            for (ompIndexType k = 0; k < elementCount; k++) {
                matOut[k] = ExponentialComplexScalar<std::complex<double>>(matIn[k]);
            }

#endif
            res = ArrayOf(NLS_DCOMPLEX, dimsA, ptrOut);
            if (res.allReal()) {
                res.promoteType(NLS_DOUBLE);
            }
        }
    } else if (classA == NLS_SINGLE || classA == NLS_SCOMPLEX) {
        Dimensions dimsA = A.dimensions();
        indexType nElementsA = A.elementCount();
        if (classA == NLS_SINGLE) {
            single* ptrOut
                = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, nElementsA, stringVector(), false);
            auto* ptrIn = (single*)A.dataPointer();
            Eigen::Map<Eigen::ArrayXf> matOut(ptrOut, nElementsA);
            Eigen::Map<Eigen::ArrayXf> matIn(ptrIn, nElementsA);
            matOut = matIn.array().exp();
            res = ArrayOf(NLS_SINGLE, dimsA, ptrOut);
        } else {
            single* ptrOut = (single*)ArrayOf::allocateArrayOf(
                NLS_SCOMPLEX, nElementsA, stringVector(), false);
            auto* Cz = reinterpret_cast<std::complex<single>*>(ptrOut);
            auto* ptrIn = (single*)A.dataPointer();
            auto* Az = reinterpret_cast<std::complex<single>*>(ptrIn);
            Eigen::Map<Eigen::ArrayXcf> matOut(Cz, nElementsA);
            Eigen::Map<Eigen::ArrayXcf> matIn(Az, nElementsA);
#if defined(_NLS_WITH_VML)
            matOut = matIn.array().exp();
#else
            ompIndexType elementCount = (ompIndexType)nElementsA;
            OMP_PARALLEL_FOR_LOOP(elementCount)
            for (ompIndexType k = 0; k < elementCount; k++) {
                matOut[k] = ExponentialComplexScalar<std::complex<single>>(matIn[k]);
            }
#endif
            res = ArrayOf(NLS_SCOMPLEX, dimsA, ptrOut);
            if (res.allReal()) {
                res.promoteType(NLS_SINGLE);
            }
        }
    } else {
        needToOverload = true;
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
