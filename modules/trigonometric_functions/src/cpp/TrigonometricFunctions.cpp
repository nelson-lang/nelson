//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _USE_MATH_DEFINES
#endif
//=============================================================================
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "lapack_eigen_config.hpp"
#if defined(_NLS_WITH_VML)
#include <mkl.h>
#endif
#include <Eigen/Dense>
#include <Eigen/Sparse>
#include <cmath>
#include <complex>
#include <functional>
#include "TrigonometricFunctions.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "Decomplexify.hpp"
#include "SparseType.hpp"
#include "SparseArrayOf.hpp"
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// The expected convention for real inputs outside [-1, 1]:
// asin(x) = pi/2 - i*acosh(x) for x > 1 ; asin(x) = -pi/2 + i*acosh(-x) for x < -1
template <class T>
static inline std::complex<T>
asinRealAsComplex(T v)
{
    if (std::isnan((double)v)) {
        return std::complex<T>(v, (T)0);
    }
    if (v > 1) {
        return std::complex<T>((T)(M_PI / 2), (T)-std::acosh((double)v));
    }
    if (v < -1) {
        return std::complex<T>((T)(-M_PI / 2), (T)std::acosh(-(double)v));
    }
    return std::complex<T>((T)std::asin((double)v), (T)0);
}
//=============================================================================
// The expected convention for real inputs outside [-1, 1]:
// acos(x) = i*acosh(x) for x > 1 ; acos(x) = pi - i*acosh(-x) for x < -1
template <class T>
static inline std::complex<T>
acosRealAsComplex(T v)
{
    if (std::isnan((double)v)) {
        return std::complex<T>(v, (T)0);
    }
    if (v > 1) {
        return std::complex<T>((T)0, (T)std::acosh((double)v));
    }
    if (v < -1) {
        return std::complex<T>((T)M_PI, (T)-std::acosh(-(double)v));
    }
    return std::complex<T>((T)std::acos((double)v), (T)0);
}
//=============================================================================
template <typename F>
static ArrayOf
SparseMapDoubleToDouble(const ArrayOf& A, F func, double implicitValue)
{
    auto* spmatDST = new Eigen::SparseMatrix<double, 0, signedIndexType>(A.rows(), A.columns());
    const auto* spmatSRC = static_cast<const Eigen::SparseMatrix<double, 0, signedIndexType>*>(
        A.sparseDataPointer());
    if (implicitValue == 0.) {
        // f(0) == 0: result stays sparse, so assemble only the mapped nonzeros.
        std::vector<Eigen::Triplet<double, signedIndexType>> triplets;
        triplets.reserve(spmatSRC->nonZeros());
        for (indexType k = 0; k < (indexType)spmatSRC->outerSize(); ++k) {
            for (Eigen::SparseMatrix<double, 0, signedIndexType>::InnerIterator it(*spmatSRC, k);
                it; ++it) {
                double value = func(it.value());
                if (value != 0.) {
                    triplets.emplace_back(it.row(), it.col(), value);
                }
            }
        }
        spmatDST->setFromTriplets(triplets.begin(), triplets.end());
    } else {
        // f(0) != 0: result is dense; pre-fill the grid then map the nonzeros.
        for (indexType col = 0; col < A.columns(); ++col) {
            for (indexType row = 0; row < A.rows(); ++row) {
                spmatDST->coeffRef(row, col) = implicitValue;
            }
        }
        for (indexType k = 0; k < (indexType)spmatSRC->outerSize(); ++k) {
            for (Eigen::SparseMatrix<double, 0, signedIndexType>::InnerIterator it(*spmatSRC, k);
                it; ++it) {
                spmatDST->coeffRef(it.row(), it.col()) = func(it.value());
            }
        }
    }
    return makeSparseResult(NLS_DOUBLE, A.dimensions(), spmatDST, true);
}
//=============================================================================
template <typename F>
static ArrayOf
SparseMapComplexToComplex(const ArrayOf& A, F func, doublecomplex implicitValue)
{
    auto* spmatDST
        = new Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>(A.rows(), A.columns());
    const auto* spmatSRC
        = static_cast<const Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>*>(
            A.sparseDataPointer());
    if (implicitValue == doublecomplex(0., 0.)) {
        std::vector<Eigen::Triplet<doublecomplex, signedIndexType>> triplets;
        triplets.reserve(spmatSRC->nonZeros());
        for (indexType k = 0; k < (indexType)spmatSRC->outerSize(); ++k) {
            for (Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>::InnerIterator it(
                     *spmatSRC, k);
                it; ++it) {
                doublecomplex value = func(it.value());
                if (value != doublecomplex(0., 0.)) {
                    triplets.emplace_back(it.row(), it.col(), value);
                }
            }
        }
        spmatDST->setFromTriplets(triplets.begin(), triplets.end());
    } else {
        for (indexType col = 0; col < A.columns(); ++col) {
            for (indexType row = 0; row < A.rows(); ++row) {
                spmatDST->coeffRef(row, col) = implicitValue;
            }
        }
        for (indexType k = 0; k < (indexType)spmatSRC->outerSize(); ++k) {
            for (Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>::InnerIterator it(
                     *spmatSRC, k);
                it; ++it) {
                spmatDST->coeffRef(it.row(), it.col()) = func(it.value());
            }
        }
    }
    return makeSparseResult(NLS_DCOMPLEX, A.dimensions(), spmatDST, true);
}
//=============================================================================
template <typename F>
static ArrayOf
SparseMapDoubleToComplex(const ArrayOf& A, F func, doublecomplex implicitValue)
{
    auto* spmatDST
        = new Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>(A.rows(), A.columns());
    const auto* spmatSRC = static_cast<const Eigen::SparseMatrix<double, 0, signedIndexType>*>(
        A.sparseDataPointer());
    if (implicitValue == doublecomplex(0., 0.)) {
        std::vector<Eigen::Triplet<doublecomplex, signedIndexType>> triplets;
        triplets.reserve(spmatSRC->nonZeros());
        for (indexType k = 0; k < (indexType)spmatSRC->outerSize(); ++k) {
            for (Eigen::SparseMatrix<double, 0, signedIndexType>::InnerIterator it(*spmatSRC, k);
                it; ++it) {
                doublecomplex value = func(it.value());
                if (value != doublecomplex(0., 0.)) {
                    triplets.emplace_back(it.row(), it.col(), value);
                }
            }
        }
        spmatDST->setFromTriplets(triplets.begin(), triplets.end());
    } else {
        for (indexType col = 0; col < A.columns(); ++col) {
            for (indexType row = 0; row < A.rows(); ++row) {
                spmatDST->coeffRef(row, col) = implicitValue;
            }
        }
        for (indexType k = 0; k < (indexType)spmatSRC->outerSize(); ++k) {
            for (Eigen::SparseMatrix<double, 0, signedIndexType>::InnerIterator it(*spmatSRC, k);
                it; ++it) {
                spmatDST->coeffRef(it.row(), it.col()) = func(it.value());
            }
        }
    }
    return makeSparseResult(NLS_DCOMPLEX, A.dimensions(), spmatDST, true);
}
//=============================================================================
static bool
SparseDoubleNeedsComplexArc(const ArrayOf& A)
{
    const auto* spmatSRC = static_cast<const Eigen::SparseMatrix<double, 0, signedIndexType>*>(
        A.sparseDataPointer());
    for (indexType k = 0; k < (indexType)spmatSRC->outerSize(); ++k) {
        for (Eigen::SparseMatrix<double, 0, signedIndexType>::InnerIterator it(*spmatSRC, k); it;
            ++it) {
            if (std::abs(it.value()) > 1) {
                return true;
            }
        }
    }
    return false;
}
//=============================================================================
ArrayOf
Cos(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    ArrayOf R;
    if (A.isEmpty()) {
        R = A;
        R.ensureSingleOwner();
        return R;
    }
    if (A.isSparse()) {
        switch (A.dataClass()) {
        case NLS_DOUBLE:
            return SparseMapDoubleToDouble(A, [](double v) { return std::cos(v); }, 1.);
        case NLS_DCOMPLEX:
            return SparseMapComplexToComplex(
                A, [](doublecomplex v) { return std::cos(v); }, doublecomplex(1., 0.));
        default:
            needToOverload = true;
            return {};
        }
    }
    switch (A.dataClass()) {
    default: {
        needToOverload = true;
    } break;
    case NLS_SCOMPLEX: {
        single* ptrR = (single*)ArrayOf::allocateArrayOf(
            NLS_SCOMPLEX, A.elementCount(), stringVector(), false);
        R = ArrayOf(NLS_SCOMPLEX, A.dimensions(), ptrR);
        auto* Az = reinterpret_cast<singlecomplex*>((single*)A.dataPointer());
        auto* Rz = reinterpret_cast<singlecomplex*>(ptrR);
#if defined(_NLS_WITH_VML)
        Eigen::Map<Eigen::VectorXcf> matEigen(Az, R.dimensions().elementCount());
        Eigen::Map<Eigen::VectorXcf> matRes(Rz, R.dimensions().elementCount());
        matRes = matEigen.array().cos();
#else
        OMP_PARALLEL_FOR_LOOP(A.elementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.elementCount(); ++k) {
            Rz[k] = std::cos(Az[k]);
        }
#endif
        if (R.allReal()) {
            R.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_SINGLE: {
        single* ptrR = (single*)ArrayOf::allocateArrayOf(
            NLS_SINGLE, A.elementCount(), stringVector(), false);
        R = ArrayOf(NLS_SINGLE, A.dimensions(), ptrR);
#if defined(_NLS_WITH_VML)
        Eigen::Map<Eigen::VectorXf> matEigen(
            (single*)A.dataPointer(), R.dimensions().elementCount());
        Eigen::Map<Eigen::VectorXf> matRes((single*)R.dataPointer(), R.dimensions().elementCount());
        matRes = matEigen.array().cos();
#else
        single* ptrA = (single*)A.dataPointer();
        OMP_PARALLEL_FOR_LOOP(A.elementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.elementCount(); ++k) {
            ptrR[k] = cos(ptrA[k]);
        }
#endif
    } break;
    case NLS_DCOMPLEX: {
        double* ptrR = (double*)ArrayOf::allocateArrayOf(
            NLS_DCOMPLEX, A.elementCount(), stringVector(), false);
        R = ArrayOf(NLS_DCOMPLEX, A.dimensions(), ptrR);
        auto* Az = reinterpret_cast<doublecomplex*>((double*)A.dataPointer());
        auto* Rz = reinterpret_cast<doublecomplex*>(ptrR);

#if defined(_NLS_WITH_VML)
        Eigen::Map<Eigen::VectorXcd> matEigen(Az, R.dimensions().elementCount());
        Eigen::Map<Eigen::VectorXcd> matRes(Rz, R.dimensions().elementCount());
        matRes = matEigen.array().cos();
#else
        OMP_PARALLEL_FOR_LOOP(A.elementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.elementCount(); ++k) {
            Rz[k] = std::cos(Az[k]);
        }
#endif
        if (R.allReal()) {
            R.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_DOUBLE: {
        double* ptrR = (double*)ArrayOf::allocateArrayOf(
            NLS_DOUBLE, A.elementCount(), stringVector(), false);
        R = ArrayOf(NLS_DOUBLE, A.dimensions(), ptrR);
#if defined(_NLS_WITH_VML)
        Eigen::Map<Eigen::VectorXd> matEigen(
            (double*)A.dataPointer(), R.dimensions().elementCount());
        Eigen::Map<Eigen::VectorXd> matRes((double*)R.dataPointer(), R.dimensions().elementCount());
        matRes = matEigen.array().cos();
#else
        double* ptrA = (double*)A.dataPointer();
        OMP_PARALLEL_FOR_LOOP(A.elementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.elementCount(); ++k) {
            ptrR[k] = cos(ptrA[k]);
        }
#endif
    } break;
    }
    return R;
}
//=============================================================================
ArrayOf
Sin(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    ArrayOf R;
    if (A.isEmpty()) {
        R = A;
        R.ensureSingleOwner();
        return R;
    }
    if (A.isSparse()) {
        switch (A.dataClass()) {
        case NLS_DOUBLE:
            return SparseMapDoubleToDouble(A, [](double v) { return std::sin(v); }, 0.);
        case NLS_DCOMPLEX:
            return SparseMapComplexToComplex(
                A, [](doublecomplex v) { return std::sin(v); }, doublecomplex(0., 0.));
        default:
            needToOverload = true;
            return {};
        }
    }
    switch (A.dataClass()) {
    default: {
        needToOverload = true;
    } break;
    case NLS_SCOMPLEX: {
        single* ptrR = (single*)ArrayOf::allocateArrayOf(
            NLS_SCOMPLEX, A.elementCount(), stringVector(), false);
        R = ArrayOf(NLS_SCOMPLEX, A.dimensions(), ptrR);
        auto* Az = reinterpret_cast<singlecomplex*>((single*)A.dataPointer());
        auto* Rz = reinterpret_cast<singlecomplex*>(ptrR);
#if defined(_NLS_WITH_VML)
        Eigen::Map<Eigen::VectorXcf> matEigen(Az, R.dimensions().elementCount());
        Eigen::Map<Eigen::VectorXcf> matRes(Rz, R.dimensions().elementCount());
        matRes = matEigen.array().sin();
#else
        OMP_PARALLEL_FOR_LOOP(A.elementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.elementCount(); ++k) {
            Rz[k] = std::sin(Az[k]);
        }
#endif
        if (R.allReal()) {
            R.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_SINGLE: {
        single* ptrR = (single*)ArrayOf::allocateArrayOf(
            NLS_SINGLE, A.elementCount(), stringVector(), false);
        R = ArrayOf(NLS_SINGLE, A.dimensions(), ptrR);
        single* ptrA = (single*)A.dataPointer();
#if defined(_NLS_WITH_VML)
        Eigen::Map<Eigen::VectorXf> matEigen(ptrA, R.dimensions().elementCount());
        Eigen::Map<Eigen::VectorXf> matRes(ptrR, R.dimensions().elementCount());
        matRes = matEigen.array().sin();
#else
        OMP_PARALLEL_FOR_LOOP(A.elementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.elementCount(); ++k) {
            ptrR[k] = std::sin(ptrA[k]);
        }
#endif
    } break;
    case NLS_DCOMPLEX: {
        double* ptrR = (double*)ArrayOf::allocateArrayOf(
            NLS_DCOMPLEX, A.elementCount(), stringVector(), false);
        R = ArrayOf(NLS_DCOMPLEX, A.dimensions(), ptrR);
        auto* Az = reinterpret_cast<doublecomplex*>((double*)A.dataPointer());
        auto* Rz = reinterpret_cast<doublecomplex*>(ptrR);
#if defined(_NLS_WITH_VML)
        Eigen::Map<Eigen::VectorXcd> matEigen(Az, R.dimensions().elementCount());
        Eigen::Map<Eigen::VectorXcd> matRes(Rz, R.dimensions().elementCount());
        matRes = matEigen.array().sin();
#else
        OMP_PARALLEL_FOR_LOOP(A.elementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.elementCount(); ++k) {
            Rz[k] = std::sin(Az[k]);
        }
#endif
        if (R.allReal()) {
            R.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_DOUBLE: {
        double* ptrR = (double*)ArrayOf::allocateArrayOf(
            NLS_DOUBLE, A.elementCount(), stringVector(), false);
        R = ArrayOf(NLS_DOUBLE, A.dimensions(), ptrR);
        double* ptrA = (double*)A.dataPointer();
#if defined(_NLS_WITH_VML)
        Eigen::Map<Eigen::VectorXd> matEigen(ptrA, R.dimensions().elementCount());
        Eigen::Map<Eigen::VectorXd> matRes(ptrR, R.dimensions().elementCount());
        matRes = matEigen.array().sin();
#else
        OMP_PARALLEL_FOR_LOOP(A.elementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.elementCount(); ++k) {
            ptrR[k] = sin(ptrA[k]);
        }
#endif
    } break;
    }
    return R;
}
//=============================================================================
ArrayOf
Tan(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    ArrayOf R;
    if (A.isEmpty()) {
        R = A;
        R.ensureSingleOwner();
        return R;
    }
    if (A.isSparse()) {
        switch (A.dataClass()) {
        case NLS_DOUBLE:
            return SparseMapDoubleToDouble(A, [](double v) { return std::tan(v); }, 0.);
        case NLS_DCOMPLEX:
            return SparseMapComplexToComplex(
                A, [](doublecomplex v) { return std::tan(v); }, doublecomplex(0., 0.));
        default:
            needToOverload = true;
            return {};
        }
    }
    switch (A.dataClass()) {
    default: {
        needToOverload = true;
    } break;
    case NLS_SCOMPLEX: {
        single* ptrR = (single*)ArrayOf::allocateArrayOf(
            NLS_SCOMPLEX, A.elementCount(), stringVector(), false);
        R = ArrayOf(NLS_SCOMPLEX, A.dimensions(), ptrR);
        auto* Az = reinterpret_cast<singlecomplex*>((single*)A.dataPointer());
        auto* Rz = reinterpret_cast<singlecomplex*>(ptrR);
#if defined(_NLS_WITH_VML)
        Eigen::Map<Eigen::VectorXcf> matEigen(Az, R.dimensions().elementCount());
        Eigen::Map<Eigen::VectorXcf> matRes(Rz, R.dimensions().elementCount());
        matRes = matEigen.array().tan();
#else
        OMP_PARALLEL_FOR_LOOP(A.elementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.elementCount(); ++k) {
            Rz[k] = std::tan(Az[k]);
        }
#endif
        if (R.allReal()) {
            R.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_SINGLE: {
        single* ptrR = (single*)ArrayOf::allocateArrayOf(
            NLS_SINGLE, A.elementCount(), stringVector(), false);
        R = ArrayOf(NLS_SINGLE, A.dimensions(), ptrR);
        single* ptrA = (single*)A.dataPointer();
#if defined(_NLS_WITH_VML)
        Eigen::Map<Eigen::VectorXf> matEigen(ptrA, R.dimensions().elementCount());
        Eigen::Map<Eigen::VectorXf> matRes(ptrR, R.dimensions().elementCount());
        matRes = matEigen.array().tan();
#else
        OMP_PARALLEL_FOR_LOOP(A.elementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.elementCount(); ++k) {
            ptrR[k] = tan(ptrA[k]);
        }
#endif
    } break;
    case NLS_DCOMPLEX: {
        double* ptrR = (double*)ArrayOf::allocateArrayOf(
            NLS_DCOMPLEX, A.elementCount(), stringVector(), false);
        R = ArrayOf(NLS_DCOMPLEX, A.dimensions(), ptrR);
        auto* Az = reinterpret_cast<doublecomplex*>((double*)A.dataPointer());
        auto* Rz = reinterpret_cast<doublecomplex*>(ptrR);
#if defined(_NLS_WITH_VML)
        Eigen::Map<Eigen::VectorXcd> matEigen(Az, R.dimensions().elementCount());
        Eigen::Map<Eigen::VectorXcd> matRes(Rz, R.dimensions().elementCount());
        matRes = matEigen.array().tan();
#else
        OMP_PARALLEL_FOR_LOOP(A.elementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.elementCount(); ++k) {
            Rz[k] = tan(Az[k]);
        }
#endif
        if (R.allReal()) {
            R.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_DOUBLE: {
        double* ptrR = (double*)ArrayOf::allocateArrayOf(
            NLS_DOUBLE, A.elementCount(), stringVector(), false);
        R = ArrayOf(NLS_DOUBLE, A.dimensions(), ptrR);
        double* ptrA = (double*)A.dataPointer();
#if defined(_NLS_WITH_VML)
        Eigen::Map<Eigen::VectorXd> matEigen(ptrA, R.dimensions().elementCount());
        Eigen::Map<Eigen::VectorXd> matRes(ptrR, R.dimensions().elementCount());
        matRes = matEigen.array().tan();
#else
        OMP_PARALLEL_FOR_LOOP(A.elementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.elementCount(); ++k) {
            ptrR[k] = tan(ptrA[k]);
        }
#endif
    } break;
    }
    return R;
}
//=============================================================================
ArrayOf
Cosh(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    ArrayOf R;
    if (A.isEmpty()) {
        R = A;
        R.ensureSingleOwner();
        return R;
    }
    if (A.isSparse()) {
        switch (A.dataClass()) {
        case NLS_DOUBLE:
            return SparseMapDoubleToDouble(A, [](double v) { return std::cosh(v); }, 1.);
        case NLS_DCOMPLEX:
            return SparseMapComplexToComplex(
                A, [](doublecomplex v) { return std::cosh(v); }, doublecomplex(1., 0.));
        default:
            needToOverload = true;
            return {};
        }
    }
    switch (A.dataClass()) {
    default: {
        needToOverload = true;
    } break;
    case NLS_SCOMPLEX: {
        single* ptrR = (single*)ArrayOf::allocateArrayOf(
            NLS_SCOMPLEX, A.elementCount(), stringVector(), false);
        R = ArrayOf(NLS_SCOMPLEX, A.dimensions(), ptrR);
        auto* Az = reinterpret_cast<singlecomplex*>((single*)A.dataPointer());
        auto* Rz = reinterpret_cast<singlecomplex*>(ptrR);
        OMP_PARALLEL_FOR_LOOP(A.elementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.elementCount(); ++k) {
            Rz[k] = std::cosh(Az[k]);
        }
        if (R.allReal()) {
            R.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_SINGLE: {
        single* ptrR = (single*)ArrayOf::allocateArrayOf(
            NLS_SINGLE, A.elementCount(), stringVector(), false);
        R = ArrayOf(NLS_SINGLE, A.dimensions(), ptrR);
        single* ptrA = (single*)A.dataPointer();
        OMP_PARALLEL_FOR_LOOP(A.elementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.elementCount(); ++k) {
            ptrR[k] = cosh(ptrA[k]);
        }
    } break;
    case NLS_DCOMPLEX: {
        double* ptrR = (double*)ArrayOf::allocateArrayOf(
            NLS_DCOMPLEX, A.elementCount(), stringVector(), false);
        R = ArrayOf(NLS_DCOMPLEX, A.dimensions(), ptrR);
        auto* Az = reinterpret_cast<doublecomplex*>((double*)A.dataPointer());
        auto* Rz = reinterpret_cast<doublecomplex*>(ptrR);
        OMP_PARALLEL_FOR_LOOP(A.elementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.elementCount(); ++k) {
            Rz[k] = std::cosh(Az[k]);
        }
        if (R.allReal()) {
            R.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_DOUBLE: {
        double* ptrR = (double*)ArrayOf::allocateArrayOf(
            NLS_DOUBLE, A.elementCount(), stringVector(), false);
        R = ArrayOf(NLS_DOUBLE, A.dimensions(), ptrR);
        double* ptrA = (double*)A.dataPointer();
        OMP_PARALLEL_FOR_LOOP(A.elementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.elementCount(); ++k) {
            ptrR[k] = cosh(ptrA[k]);
        }
    } break;
    }
    return R;
}
//=============================================================================
ArrayOf
Sinh(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    ArrayOf R;
    if (A.isEmpty()) {
        R = A;
        R.ensureSingleOwner();
        return R;
    }
    if (A.isSparse()) {
        switch (A.dataClass()) {
        case NLS_DOUBLE:
            return SparseMapDoubleToDouble(A, [](double v) { return std::sinh(v); }, 0.);
        case NLS_DCOMPLEX:
            return SparseMapComplexToComplex(
                A, [](doublecomplex v) { return std::sinh(v); }, doublecomplex(0., 0.));
        default:
            needToOverload = true;
            return {};
        }
    }
    switch (A.dataClass()) {
    default: {
        needToOverload = true;
    } break;
    case NLS_SCOMPLEX: {
        single* ptrR = (single*)ArrayOf::allocateArrayOf(
            NLS_SCOMPLEX, A.elementCount(), stringVector(), false);
        R = ArrayOf(NLS_SCOMPLEX, A.dimensions(), ptrR);
        auto* Az = reinterpret_cast<singlecomplex*>((single*)A.dataPointer());
        auto* Rz = reinterpret_cast<singlecomplex*>(ptrR);
        OMP_PARALLEL_FOR_LOOP(A.elementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.elementCount(); ++k) {
            Rz[k] = std::sinh(Az[k]);
        }
        if (R.allReal()) {
            R.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_SINGLE: {
        single* ptrR = (single*)ArrayOf::allocateArrayOf(
            NLS_SINGLE, A.elementCount(), stringVector(), false);
        R = ArrayOf(NLS_SINGLE, A.dimensions(), ptrR);
        single* ptrA = (single*)A.dataPointer();
        OMP_PARALLEL_FOR_LOOP(A.elementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.elementCount(); ++k) {
            ptrR[k] = sinh(ptrA[k]);
        }
    } break;
    case NLS_DCOMPLEX: {
        double* ptrR = (double*)ArrayOf::allocateArrayOf(
            NLS_DCOMPLEX, A.elementCount(), stringVector(), false);
        R = ArrayOf(NLS_DCOMPLEX, A.dimensions(), ptrR);
        auto* Az = reinterpret_cast<doublecomplex*>((double*)A.dataPointer());
        auto* Rz = reinterpret_cast<doublecomplex*>(ptrR);
        OMP_PARALLEL_FOR_LOOP(A.elementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.elementCount(); ++k) {
            Rz[k] = std::sinh(Az[k]);
        }
        if (R.allReal()) {
            R.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_DOUBLE: {
        double* ptrR = (double*)ArrayOf::allocateArrayOf(
            NLS_DOUBLE, A.elementCount(), stringVector(), false);
        R = ArrayOf(NLS_DOUBLE, A.dimensions(), ptrR);
        double* ptrA = (double*)A.dataPointer();
        OMP_PARALLEL_FOR_LOOP(A.elementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.elementCount(); ++k) {
            ptrR[k] = sinh(ptrA[k]);
        }
    } break;
    }
    return R;
}
//=============================================================================
ArrayOf
Tanh(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    ArrayOf R;
    if (A.isEmpty()) {
        R = A;
        R.ensureSingleOwner();
        return R;
    }
    if (A.isSparse()) {
        switch (A.dataClass()) {
        case NLS_DOUBLE:
            return SparseMapDoubleToDouble(A, [](double v) { return std::tanh(v); }, 0.);
        case NLS_DCOMPLEX:
            return SparseMapComplexToComplex(
                A, [](doublecomplex v) { return std::tanh(v); }, doublecomplex(0., 0.));
        default:
            needToOverload = true;
            return {};
        }
    }
    switch (A.dataClass()) {
    default: {
        needToOverload = true;
    } break;
    case NLS_SCOMPLEX: {
        single* ptrR = (single*)ArrayOf::allocateArrayOf(
            NLS_SCOMPLEX, A.elementCount(), stringVector(), false);
        R = ArrayOf(NLS_SCOMPLEX, A.dimensions(), ptrR);
        auto* Az = reinterpret_cast<singlecomplex*>((single*)A.dataPointer());
        auto* Rz = reinterpret_cast<singlecomplex*>(ptrR);
        OMP_PARALLEL_FOR_LOOP(A.elementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.elementCount(); ++k) {
            Rz[k] = std::tanh(Az[k]);
        }
        if (R.allReal()) {
            R.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_SINGLE: {
        single* ptrR = (single*)ArrayOf::allocateArrayOf(
            NLS_SINGLE, A.elementCount(), stringVector(), false);
        R = ArrayOf(NLS_SINGLE, A.dimensions(), ptrR);
        single* ptrA = (single*)A.dataPointer();
        OMP_PARALLEL_FOR_LOOP(A.elementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.elementCount(); ++k) {
            ptrR[k] = tanh(ptrA[k]);
        }
    } break;
    case NLS_DCOMPLEX: {
        double* ptrR = (double*)ArrayOf::allocateArrayOf(
            NLS_DCOMPLEX, A.elementCount(), stringVector(), false);
        R = ArrayOf(NLS_DCOMPLEX, A.dimensions(), ptrR);
        auto* Az = reinterpret_cast<doublecomplex*>((double*)A.dataPointer());
        auto* Rz = reinterpret_cast<doublecomplex*>(ptrR);
        OMP_PARALLEL_FOR_LOOP(A.elementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.elementCount(); ++k) {
            Rz[k] = std::tanh(Az[k]);
        }
        if (R.allReal()) {
            R.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_DOUBLE: {
        double* ptrR = (double*)ArrayOf::allocateArrayOf(
            NLS_DOUBLE, A.elementCount(), stringVector(), false);
        R = ArrayOf(NLS_DOUBLE, A.dimensions(), ptrR);
        double* ptrA = (double*)A.dataPointer();
        OMP_PARALLEL_FOR_LOOP(A.elementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.elementCount(); ++k) {
            ptrR[k] = tanh(ptrA[k]);
        }
    } break;
    }
    return R;
}
//=============================================================================
ArrayOf
Acos(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    ArrayOf R;
    if (A.isEmpty()) {
        R = A;
        R.ensureSingleOwner();
        return R;
    }
    if (A.isSparse()) {
        switch (A.dataClass()) {
        case NLS_DOUBLE:
            if (SparseDoubleNeedsComplexArc(A)) {
                return SparseMapDoubleToComplex(
                    A, [](double v) { return acosRealAsComplex<double>(v); },
                    doublecomplex(M_PI / 2., 0.));
            }
            return SparseMapDoubleToDouble(A, [](double v) { return std::acos(v); }, M_PI / 2.);
        case NLS_DCOMPLEX:
            return SparseMapComplexToComplex(
                A, [](doublecomplex v) { return std::acos(v); }, doublecomplex(M_PI / 2., 0.));
        default:
            needToOverload = true;
            return {};
        }
    }
    switch (A.dataClass()) {
    default: {
        needToOverload = true;
    } break;
    case NLS_SCOMPLEX: {
        single* ptrR = (single*)ArrayOf::allocateArrayOf(
            NLS_SCOMPLEX, A.elementCount(), stringVector(), false);
        R = ArrayOf(NLS_SCOMPLEX, A.dimensions(), ptrR);
        auto* Az = reinterpret_cast<singlecomplex*>((single*)A.dataPointer());
        auto* Rz = reinterpret_cast<singlecomplex*>(ptrR);
#if defined(_NLS_WITH_VML)
        Eigen::Map<Eigen::VectorXcf> matEigen(Az, R.dimensions().elementCount());
        Eigen::Map<Eigen::VectorXcf> matRes(Rz, R.dimensions().elementCount());
        matRes = matEigen.array().acos();
#else
        OMP_PARALLEL_FOR_LOOP(A.elementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.elementCount(); ++k) {
            Rz[k] = std::acos(Az[k]);
        }
#endif
        if (R.allReal()) {
            R.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_SINGLE: {
        auto* ptrA = (single*)A.dataPointer();
        bool needToConvertAsComplex = false;
        indexType elementCount = A.elementCount();
        for (indexType i = 0; i < elementCount; i++) {
            if (std::fabs(ptrA[i]) > 1) {
                needToConvertAsComplex = true;
                break;
            }
        }
        if (needToConvertAsComplex) {
            single* ptrR = (single*)ArrayOf::allocateArrayOf(
                NLS_SCOMPLEX, A.elementCount(), stringVector(), false);
            R = ArrayOf(NLS_SCOMPLEX, A.dimensions(), ptrR);
            auto* Rz = reinterpret_cast<singlecomplex*>(ptrR);
            OMP_PARALLEL_FOR_LOOP(A.elementCount())
            for (ompIndexType k = 0; k < (ompIndexType)A.elementCount(); ++k) {
                Rz[k] = acosRealAsComplex<single>(ptrA[k]);
            }
        } else {
            single* ptrR = (single*)ArrayOf::allocateArrayOf(
                NLS_SINGLE, A.elementCount(), stringVector(), false);
            R = ArrayOf(NLS_SINGLE, A.dimensions(), ptrR);
            single* ptrA = (single*)A.dataPointer();
#if defined(_NLS_WITH_VML)
            Eigen::Map<Eigen::VectorXf> matEigen(ptrA, R.dimensions().elementCount());
            Eigen::Map<Eigen::VectorXf> matRes(ptrR, R.dimensions().elementCount());
            matRes = matEigen.array().acos();
#else
            OMP_PARALLEL_FOR_LOOP(A.elementCount())
            for (ompIndexType k = 0; k < (ompIndexType)A.elementCount(); ++k) {
                ptrR[k] = acos(ptrA[k]);
            }
#endif
        }
    } break;
    case NLS_DCOMPLEX: {
        double* ptrR = (double*)ArrayOf::allocateArrayOf(
            NLS_DCOMPLEX, A.elementCount(), stringVector(), false);
        R = ArrayOf(NLS_DCOMPLEX, A.dimensions(), ptrR);
        auto* Az = reinterpret_cast<doublecomplex*>((double*)A.dataPointer());
        auto* Rz = reinterpret_cast<doublecomplex*>(ptrR);
#if defined(_NLS_WITH_VML)
        Eigen::Map<Eigen::VectorXcd> matEigen(Az, R.dimensions().elementCount());
        Eigen::Map<Eigen::VectorXcd> matRes(Rz, R.dimensions().elementCount());
        matRes = matEigen.array().acos();
#else
        OMP_PARALLEL_FOR_LOOP(A.elementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.elementCount(); ++k) {
            Rz[k] = std::acos(Az[k]);
        }
#endif
        if (R.allReal()) {
            R.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_DOUBLE: {
        auto* ptrA = (double*)A.dataPointer();
        bool needToConvertAsComplex = false;
        indexType elementCount = A.elementCount();
        for (indexType i = 0; i < elementCount; i++) {
            if (std::abs(ptrA[i]) > 1) {
                needToConvertAsComplex = true;
                break;
            }
        }
        if (needToConvertAsComplex) {
            double* ptrR = (double*)ArrayOf::allocateArrayOf(
                NLS_DCOMPLEX, A.elementCount(), stringVector(), false);
            R = ArrayOf(NLS_DCOMPLEX, A.dimensions(), ptrR);
            auto* Rz = reinterpret_cast<doublecomplex*>(ptrR);
            OMP_PARALLEL_FOR_LOOP(A.elementCount())
            for (ompIndexType k = 0; k < (ompIndexType)A.elementCount(); ++k) {
                Rz[k] = acosRealAsComplex<double>(ptrA[k]);
            }
        } else {
            double* ptrR = (double*)ArrayOf::allocateArrayOf(
                NLS_DOUBLE, A.elementCount(), stringVector(), false);
            R = ArrayOf(NLS_DOUBLE, A.dimensions(), ptrR);
            double* ptrA = (double*)A.dataPointer();
#if defined(_NLS_WITH_VML)
            Eigen::Map<Eigen::VectorXd> matEigen(ptrA, R.dimensions().elementCount());
            Eigen::Map<Eigen::VectorXd> matRes(ptrR, R.dimensions().elementCount());
            matRes = matEigen.array().acos();
#else
            OMP_PARALLEL_FOR_LOOP(A.elementCount())
            for (ompIndexType k = 0; k < (ompIndexType)A.elementCount(); ++k) {
                ptrR[k] = acos(ptrA[k]);
            }
#endif
        }
    } break;
    }
    return R;
}
//=============================================================================
ArrayOf
Asin(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    ArrayOf R;
    if (A.isEmpty()) {
        R = A;
        R.ensureSingleOwner();
        return R;
    }
    if (A.isSparse()) {
        switch (A.dataClass()) {
        case NLS_DOUBLE:
            if (SparseDoubleNeedsComplexArc(A)) {
                return SparseMapDoubleToComplex(
                    A, [](double v) { return asinRealAsComplex<double>(v); },
                    doublecomplex(0., 0.));
            }
            return SparseMapDoubleToDouble(A, [](double v) { return std::asin(v); }, 0.);
        case NLS_DCOMPLEX:
            return SparseMapComplexToComplex(
                A, [](doublecomplex v) { return std::asin(v); }, doublecomplex(0., 0.));
        default:
            needToOverload = true;
            return {};
        }
    }
    switch (A.dataClass()) {
    default: {
        needToOverload = true;
    } break;
    case NLS_SCOMPLEX: {
        single* ptrR = (single*)ArrayOf::allocateArrayOf(
            NLS_SCOMPLEX, A.elementCount(), stringVector(), false);
        R = ArrayOf(NLS_SCOMPLEX, A.dimensions(), ptrR);
        auto* Az = reinterpret_cast<singlecomplex*>((single*)A.dataPointer());
        auto* Rz = reinterpret_cast<singlecomplex*>(ptrR);
#if defined(_NLS_WITH_VML)
        Eigen::Map<Eigen::VectorXcf> matEigen(Az, R.dimensions().elementCount());
        Eigen::Map<Eigen::VectorXcf> matRes(Rz, R.dimensions().elementCount());
        matRes = matEigen.array().asin();
#else
        OMP_PARALLEL_FOR_LOOP(A.elementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.elementCount(); ++k) {
            Rz[k] = std::asin(Az[k]);
        }
#endif
        if (R.allReal()) {
            R.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_SINGLE: {
        auto* ptrA = (single*)A.dataPointer();
        bool needToConvertAsComplex = false;
        indexType elementCount = A.elementCount();
        for (indexType i = 0; i < elementCount; i++) {
            if (std::fabs(ptrA[i]) > 1) {
                needToConvertAsComplex = true;
                break;
            }
        }
        if (needToConvertAsComplex) {
            single* ptrR = (single*)ArrayOf::allocateArrayOf(
                NLS_SCOMPLEX, A.elementCount(), stringVector(), false);
            R = ArrayOf(NLS_SCOMPLEX, A.dimensions(), ptrR);
            auto* Rz = reinterpret_cast<singlecomplex*>(ptrR);
            OMP_PARALLEL_FOR_LOOP(A.elementCount())
            for (ompIndexType k = 0; k < (ompIndexType)A.elementCount(); ++k) {
                Rz[k] = asinRealAsComplex<single>(ptrA[k]);
            }
        } else {
            single* ptrR = (single*)ArrayOf::allocateArrayOf(
                NLS_SINGLE, A.elementCount(), stringVector(), false);
            R = ArrayOf(NLS_SINGLE, A.dimensions(), ptrR);
            single* ptrA = (single*)A.dataPointer();
#if defined(_NLS_WITH_VML)
            Eigen::Map<Eigen::VectorXf> matEigen(ptrA, R.dimensions().elementCount());
            Eigen::Map<Eigen::VectorXf> matRes(ptrR, R.dimensions().elementCount());
            matRes = matEigen.array().asin();
#else
            OMP_PARALLEL_FOR_LOOP(A.elementCount())
            for (ompIndexType k = 0; k < (ompIndexType)A.elementCount(); ++k) {
                ptrR[k] = asin(ptrA[k]);
            }
#endif
        }
    } break;
    case NLS_DCOMPLEX: {
        double* ptrR = (double*)ArrayOf::allocateArrayOf(
            NLS_DCOMPLEX, A.elementCount(), stringVector(), false);
        R = ArrayOf(NLS_DCOMPLEX, A.dimensions(), ptrR);
        auto* Az = reinterpret_cast<doublecomplex*>((double*)A.dataPointer());
        auto* Rz = reinterpret_cast<doublecomplex*>(ptrR);
#if defined(_NLS_WITH_VML)
        Eigen::Map<Eigen::VectorXcd> matEigen(Az, R.dimensions().elementCount());
        Eigen::Map<Eigen::VectorXcd> matRes(Rz, R.dimensions().elementCount());
        matRes = matEigen.array().asin();
#else
        OMP_PARALLEL_FOR_LOOP(A.elementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.elementCount(); ++k) {
            Rz[k] = std::asin(Az[k]);
        }
#endif
        if (R.allReal()) {
            R.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_DOUBLE: {
        auto* ptrA = (double*)A.dataPointer();
        bool needToConvertAsComplex = false;
        indexType elementCount = A.elementCount();
        for (indexType i = 0; i < elementCount; i++) {
            if (std::abs(ptrA[i]) > 1) {
                needToConvertAsComplex = true;
                break;
            }
        }
        if (needToConvertAsComplex) {
            double* ptrR = (double*)ArrayOf::allocateArrayOf(
                NLS_DCOMPLEX, A.elementCount(), stringVector(), false);
            R = ArrayOf(NLS_DCOMPLEX, A.dimensions(), ptrR);
            auto* Rz = reinterpret_cast<doublecomplex*>(ptrR);
            OMP_PARALLEL_FOR_LOOP(A.elementCount())
            for (ompIndexType k = 0; k < (ompIndexType)A.elementCount(); ++k) {
                Rz[k] = asinRealAsComplex<double>(ptrA[k]);
            }
        } else {
            double* ptrR = (double*)ArrayOf::allocateArrayOf(
                NLS_DOUBLE, A.elementCount(), stringVector(), false);
            R = ArrayOf(NLS_DOUBLE, A.dimensions(), ptrR);
            double* ptrA = (double*)A.dataPointer();
#if defined(_NLS_WITH_VML)
            Eigen::Map<Eigen::VectorXd> matEigen(ptrA, R.dimensions().elementCount());
            Eigen::Map<Eigen::VectorXd> matRes(ptrR, R.dimensions().elementCount());
            matRes = matEigen.array().asin();
#else
            OMP_PARALLEL_FOR_LOOP(A.elementCount())
            for (ompIndexType k = 0; k < (ompIndexType)A.elementCount(); ++k) {
                ptrR[k] = asin(ptrA[k]);
            }
#endif
        }
    } break;
    }
    return R;
}
//=============================================================================
ArrayOf
Atan(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    ArrayOf R;
    if (A.isEmpty()) {
        R = A;
        R.ensureSingleOwner();
        return R;
    }
    if (A.isSparse()) {
        switch (A.dataClass()) {
        case NLS_DOUBLE:
            return SparseMapDoubleToDouble(A, [](double v) { return std::atan(v); }, 0.);
        case NLS_DCOMPLEX:
            return SparseMapComplexToComplex(
                A, [](doublecomplex v) { return std::atan(v); }, doublecomplex(0., 0.));
        default:
            needToOverload = true;
            return {};
        }
    }
    switch (A.dataClass()) {
    default: {
        needToOverload = true;
    } break;
    case NLS_SCOMPLEX: {
        single* ptrR = (single*)ArrayOf::allocateArrayOf(
            NLS_SCOMPLEX, A.elementCount(), stringVector(), false);
        R = ArrayOf(NLS_SCOMPLEX, A.dimensions(), ptrR);
        auto* Az = reinterpret_cast<singlecomplex*>((single*)A.dataPointer());
        auto* Rz = reinterpret_cast<singlecomplex*>(ptrR);
        OMP_PARALLEL_FOR_LOOP(A.elementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.elementCount(); ++k) {
            Rz[k] = std::atan(Az[k]);
        }
        if (R.allReal()) {
            R.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_SINGLE: {
        single* ptrR = (single*)ArrayOf::allocateArrayOf(
            NLS_SINGLE, A.elementCount(), stringVector(), false);
        R = ArrayOf(NLS_SINGLE, A.dimensions(), ptrR);
        single* ptrA = (single*)A.dataPointer();
        OMP_PARALLEL_FOR_LOOP(A.elementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.elementCount(); ++k) {
            ptrR[k] = atan(ptrA[k]);
        }
    } break;
    case NLS_DCOMPLEX: {
        double* ptrR = (double*)ArrayOf::allocateArrayOf(
            NLS_DCOMPLEX, A.elementCount(), stringVector(), false);
        R = ArrayOf(NLS_DCOMPLEX, A.dimensions(), ptrR);
        auto* Az = reinterpret_cast<doublecomplex*>((double*)A.dataPointer());
        auto* Rz = reinterpret_cast<doublecomplex*>(ptrR);
        OMP_PARALLEL_FOR_LOOP(A.elementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.elementCount(); ++k) {
            Rz[k] = std::atan(Az[k]);
        }
        if (R.allReal()) {
            R.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_DOUBLE: {
        double* ptrR = (double*)ArrayOf::allocateArrayOf(
            NLS_DOUBLE, A.elementCount(), stringVector(), false);
        R = ArrayOf(NLS_DOUBLE, A.dimensions(), ptrR);
        double* ptrA = (double*)A.dataPointer();
        OMP_PARALLEL_FOR_LOOP(A.elementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.elementCount(); ++k) {
            ptrR[k] = atan(ptrA[k]);
        }
    } break;
    }
    return R;
}
//=============================================================================
template <typename T>
static inline void
arctanh(bool asComplex, T xr, T xi, T& yr, T& yi)
{
    if ((xr >= 0 && std::isinf(xr)) && (xi == 0)) {
        yr = 0;
        yi = (T)M_PI / 2.0;
        return;
    }
    if ((xr < 0 && std::isinf(xr)) && (xi == 0)) {
        yr = 0;
        yi = -(T)M_PI / 2.0;
        return;
    }
    std::complex<T> v(xr, xi);
    std::complex<T> r = std::atanh(v);
    yr = r.real();
    yi = r.imag();

    if (!asComplex) {
        if (std::isnan(yr) && std::isnan(yi)) {
            yi = 0;
        }
    }
}
//=============================================================================
ArrayOf
Atanh(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    ArrayOf R;
    if (A.isEmpty()) {
        R = A;
        R.ensureSingleOwner();
        return R;
    }
    if (A.isSparse()) {
        switch (A.dataClass()) {
        case NLS_DOUBLE:
            return SparseMapDoubleToComplex(
                A, [](double v) { return std::atanh(doublecomplex(v, 0.)); },
                doublecomplex(0., 0.));
        case NLS_DCOMPLEX:
            return SparseMapComplexToComplex(
                A, [](doublecomplex v) { return std::atanh(v); }, doublecomplex(0., 0.));
        default:
            needToOverload = true;
            return {};
        }
    }
    switch (A.dataClass()) {
    default: {
        needToOverload = true;
    } break;
    case NLS_SCOMPLEX: {
        single* ptrR = (single*)ArrayOf::allocateArrayOf(NLS_SCOMPLEX, A.elementCount());
        R = ArrayOf(NLS_SCOMPLEX, A.dimensions(), ptrR);
        single* ptrA = (single*)A.dataPointer();
        OMP_PARALLEL_FOR_LOOP(A.elementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.elementCount(); k = k + 2) {
            arctanh<single>(true, ptrA[k], ptrA[k + 1], ptrR[k], ptrR[k + 1]);
        }
    } break;
    case NLS_SINGLE: {
        single* ptrR = (single*)ArrayOf::allocateArrayOf(NLS_SCOMPLEX, A.elementCount());
        R = ArrayOf(NLS_SCOMPLEX, A.dimensions(), ptrR);
        single* ptrA = (single*)A.dataPointer();
        OMP_PARALLEL_FOR_LOOP(A.elementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.elementCount(); ++k) {
            arctanh<single>(false, ptrA[k], 0, ptrR[2 * k], ptrR[(2 * k) + 1]);
        }
    } break;
    case NLS_DCOMPLEX: {
        double* ptrR = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, A.elementCount());
        R = ArrayOf(NLS_DCOMPLEX, A.dimensions(), ptrR);
        double* ptrA = (double*)A.dataPointer();
        OMP_PARALLEL_FOR_LOOP(A.elementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.elementCount(); k = k + 2) {
            arctanh<double>(true, ptrA[k], ptrA[k + 1], ptrR[k], ptrR[k + 1]);
        }
    } break;
    case NLS_DOUBLE: {
        double* ptrR = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, A.elementCount());
        R = ArrayOf(NLS_DCOMPLEX, A.dimensions(), ptrR);
        double* ptrA = (double*)A.dataPointer();
        OMP_PARALLEL_FOR_LOOP(A.elementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.elementCount(); ++k) {
            arctanh<double>(false, ptrA[k], 0, ptrR[2 * k], ptrR[(2 * k) + 1]);
        }
    } break;
    }
    return decomplexify(R);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
