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
#include <limits>
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
// Complex input: a zero imaginary part follows the real rule, a NaN part gives NaN + NaNi.
template <class T>
static inline std::complex<T>
asinComplexValue(std::complex<T> z)
{
    if (z.imag() == 0) {
        return asinRealAsComplex<T>(z.real());
    }
    if (std::isnan(z.real()) || std::isnan(z.imag())) {
        T nan = std::numeric_limits<T>::quiet_NaN();
        return std::complex<T>(nan, nan);
    }
    return std::asin(z);
}
//=============================================================================
// Same rules as asinComplexValue; both parts infinite give pi/2 with an infinite imaginary part.
template <class T>
static inline std::complex<T>
acosComplexValue(std::complex<T> z)
{
    if (z.imag() == 0) {
        return acosRealAsComplex<T>(z.real());
    }
    if (std::isnan(z.real()) || std::isnan(z.imag())) {
        T nan = std::numeric_limits<T>::quiet_NaN();
        return std::complex<T>(nan, nan);
    }
    if (std::isinf(z.real()) && std::isinf(z.imag())) {
        return std::complex<T>((T)(M_PI / 2), -z.imag());
    }
    return std::acos(z);
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
                A, [](doublecomplex v) { return acosComplexValue<double>(v); },
                doublecomplex(M_PI / 2., 0.));
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
            Rz[k] = acosComplexValue<single>(Az[k]);
        }
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
        OMP_PARALLEL_FOR_LOOP(A.elementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.elementCount(); ++k) {
            Rz[k] = acosComplexValue<double>(Az[k]);
        }
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
                A, [](doublecomplex v) { return asinComplexValue<double>(v); }, doublecomplex(0., 0.));
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
            Rz[k] = asinComplexValue<single>(Az[k]);
        }
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
        OMP_PARALLEL_FOR_LOOP(A.elementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.elementCount(); ++k) {
            Rz[k] = asinComplexValue<double>(Az[k]);
        }
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
// Complex value with a NaN part: NaN + 0i stays NaN + 0i, anything else is NaN + NaNi.
template <class T>
static inline std::complex<T>
complexNaNResult(std::complex<T> z)
{
    T nan = std::numeric_limits<T>::quiet_NaN();
    return std::complex<T>(nan, z.imag() == 0 ? (T)0 : nan);
}
//=============================================================================
// Zero parts of a complex result: the real one is -0 only for a negative input real part,
// the imaginary one is +0.
template <class T>
static inline std::complex<T>
withInputSignedZeros(std::complex<T> w, std::complex<T> z)
{
    T re = w.real() == 0 ? (z.real() < 0 ? (T)-0.0 : (T)0) : w.real();
    return std::complex<T>(re, w.imag() == 0 ? (T)0 : w.imag());
}
//=============================================================================
// Maps a dense real or complex array. Real input gives a complex result only when one
// value is outside the real domain; complex input gives a real result when it can.
template <class T, class InDomain, class RealFn, class RealAsComplexFn, class ComplexFn>
static ArrayOf
mapUnaryDense(const ArrayOf& A, NelsonType realClass, NelsonType complexClass,
    InDomain inDomain, RealFn realFn, RealAsComplexFn realAsComplexFn, ComplexFn complexFn)
{
    Dimensions dims = A.dimensions();
    ompIndexType n = (ompIndexType)dims.elementCount();
    if (A.dataClass() == realClass) {
        const T* x = (const T*)A.dataPointer();
        bool allInDomain = true;
        for (ompIndexType k = 0; k < n; ++k) {
            if (!inDomain(x[k])) {
                allInDomain = false;
                break;
            }
        }
        if (allInDomain) {
            T* y = (T*)ArrayOf::allocateArrayOf(realClass, n, stringVector(), false);
            OMP_PARALLEL_FOR_LOOP(n)
            for (ompIndexType k = 0; k < n; ++k) {
                y[k] = realFn(x[k]);
            }
            return ArrayOf(realClass, dims, y);
        }
        T* y = (T*)ArrayOf::allocateArrayOf(complexClass, n, stringVector(), false);
        auto* yz = reinterpret_cast<std::complex<T>*>(y);
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType k = 0; k < n; ++k) {
            yz[k] = realAsComplexFn(x[k]);
        }
        return ArrayOf(complexClass, dims, y);
    }
    const auto* xz = reinterpret_cast<const std::complex<T>*>(A.dataPointer());
    T* y = (T*)ArrayOf::allocateArrayOf(complexClass, n, stringVector(), false);
    auto* yz = reinterpret_cast<std::complex<T>*>(y);
    OMP_PARALLEL_FOR_LOOP(n)
    for (ompIndexType k = 0; k < n; ++k) {
        yz[k] = complexFn(xz[k]);
    }
    ArrayOf R(complexClass, dims, y);
    if (R.allReal()) {
        R.promoteType(realClass);
    }
    return R;
}
//=============================================================================
// Runs one kernel namespace (inDomain, real, realAsComplex, complexValue) on a dense array.
template <class T, class Kernel>
static ArrayOf
mapKernelDense(const ArrayOf& A, NelsonType realClass, NelsonType complexClass)
{
    return mapUnaryDense<T>(
        A, realClass, complexClass, [](T x) { return Kernel::template inDomain<T>(x); },
        [](T x) { return Kernel::template real<T>(x); },
        [](T x) { return Kernel::template realAsComplex<T>(x); },
        [](std::complex<T> z) { return Kernel::template complexValue<T>(z); });
}
//=============================================================================
template <class Kernel>
static ArrayOf
mapKernel(const ArrayOf& A, bool& needToOverload, doublecomplex sparseImplicitValue)
{
    needToOverload = false;
    if (A.isEmpty()) {
        ArrayOf R = A;
        R.ensureSingleOwner();
        return R;
    }
    if (A.isSparse()) {
        switch (A.dataClass()) {
        case NLS_DOUBLE:
            if (sparseImplicitValue.imag() == 0 && Kernel::realOnly) {
                return SparseMapDoubleToDouble(
                    A, [](double v) { return Kernel::template real<double>(v); },
                    sparseImplicitValue.real());
            }
            return SparseMapDoubleToComplex(
                A, [](double v) { return Kernel::template realAsComplex<double>(v); },
                sparseImplicitValue);
        case NLS_DCOMPLEX:
            return SparseMapComplexToComplex(
                A, [](doublecomplex v) { return Kernel::template complexValue<double>(v); },
                sparseImplicitValue);
        default:
            needToOverload = true;
            return {};
        }
    }
    switch (A.dataClass()) {
    case NLS_DOUBLE:
    case NLS_DCOMPLEX:
        return mapKernelDense<double, Kernel>(A, NLS_DOUBLE, NLS_DCOMPLEX);
    case NLS_SINGLE:
    case NLS_SCOMPLEX:
        return mapKernelDense<single, Kernel>(A, NLS_SINGLE, NLS_SCOMPLEX);
    default:
        needToOverload = true;
        return {};
    }
}
//=============================================================================
struct AtanhKernel
{
    static constexpr bool realOnly = false;
    template <class T>
    static bool
    inDomain(T x)
    {
        return !(std::fabs(x) > 1);
    }
    template <class T>
    static T
    real(T x)
    {
        return std::atanh(x);
    }
    // Outside [-1, 1] the imaginary part takes the sign of x: atanh(-Inf) is -0 - pi/2 i.
    template <class T>
    static std::complex<T>
    realAsComplex(T x)
    {
        if (std::fabs(x) > 1) {
            return std::complex<T>(std::atanh((T)1 / x), std::copysign((T)(M_PI / 2), x));
        }
        return std::complex<T>(std::atanh(x), (T)0);
    }
    template <class T>
    static std::complex<T>
    complexValue(std::complex<T> z)
    {
        if (z.imag() == 0) {
            return realAsComplex<T>(z.real());
        }
        if (std::isnan(z.real()) || std::isnan(z.imag())) {
            return complexNaNResult<T>(z);
        }
        return withInputSignedZeros<T>(std::atanh(z), z);
    }
};
//=============================================================================
struct AsinhKernel
{
    static constexpr bool realOnly = true;
    template <class T>
    static bool
    inDomain(T)
    {
        return true;
    }
    template <class T>
    static T
    real(T x)
    {
        return std::asinh(x);
    }
    template <class T>
    static std::complex<T>
    realAsComplex(T x)
    {
        return std::complex<T>(std::asinh(x), (T)0);
    }
    template <class T>
    static std::complex<T>
    complexValue(std::complex<T> z)
    {
        if (z.imag() == 0) {
            return realAsComplex<T>(z.real());
        }
        if (std::isnan(z.real()) || std::isnan(z.imag())) {
            if (z.real() == 0) {
                return std::complex<T>((T)0, std::numeric_limits<T>::quiet_NaN());
            }
            return complexNaNResult<T>(z);
        }
        // at infinity on the imaginary axis both parts follow the sign of the imaginary part
        if (z.real() == 0 && std::isinf(z.imag())) {
            return std::complex<T>(z.imag(), std::copysign((T)(M_PI / 2), z.imag()));
        }
        return withInputSignedZeros<T>(std::asinh(z), z);
    }
};
//=============================================================================
struct AcoshKernel
{
    static constexpr bool realOnly = false;
    template <class T>
    static bool
    inDomain(T x)
    {
        return x >= 1 || std::isnan(x);
    }
    template <class T>
    static T
    real(T x)
    {
        return std::acosh(x);
    }
    // Below 1: i*acos(x) on [-1, 1), acosh(-x) + pi*i below -1.
    template <class T>
    static std::complex<T>
    realAsComplex(T x)
    {
        if (x >= 1 || std::isnan(x)) {
            return std::complex<T>(std::acosh(x), (T)0);
        }
        if (x >= -1) {
            return std::complex<T>((T)0, std::acos(x));
        }
        return std::complex<T>(std::acosh(-x), (T)M_PI);
    }
    template <class T>
    static std::complex<T>
    complexValue(std::complex<T> z)
    {
        if (z.imag() == 0) {
            return realAsComplex<T>(z.real());
        }
        if (std::isnan(z.real()) || std::isnan(z.imag())) {
            return complexNaNResult<T>(z);
        }
        if (std::isinf(z.real()) && std::isinf(z.imag())) {
            return std::complex<T>(
                std::numeric_limits<T>::infinity(), std::copysign((T)(M_PI / 2), z.imag()));
        }
        return withInputSignedZeros<T>(std::acosh(z), z);
    }
};
//=============================================================================
// Sine and cosine of an angle in degrees: exact range reduction, quadrant choice and
// a two-part pi/180 so that sind(30) is 0.5 and multiples of 90 are exact.
static inline void
sinCosDegrees(double x, double& s, double& c)
{
    const double degToRadHigh = 0.017453292519943295;
    const double degToRadLow = 2.9486522708701687e-19;
    double r = std::fmod(x, 360.0);
    double q = std::round(r / 90.0);
    double t = r - 90.0 * q;
    double hi = t * degToRadHigh;
    double lo = std::fma(t, degToRadHigh, -hi) + t * degToRadLow;
    double sv = std::sin(hi) + lo * (1.0 - 0.5 * hi * hi);
    double cv = std::cos(hi) - lo * hi;
    switch (static_cast<int>(q) & 3) {
    case 0:
        s = sv;
        c = cv;
        break;
    case 1:
        s = cv;
        c = -sv;
        break;
    case 2:
        s = -sv;
        c = -cv;
        break;
    default:
        s = -cv;
        c = sv;
        break;
    }
}
//=============================================================================
// Zero sines take the sign of x, zero cosines are +0.
static inline double
sindValue(double x)
{
    if (!std::isfinite(x)) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    double s;
    double c;
    sinCosDegrees(x, s, c);
    return s == 0 ? std::copysign(0.0, x) : s;
}
//=============================================================================
static inline double
cosdValue(double x)
{
    if (!std::isfinite(x)) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    double s;
    double c;
    sinCosDegrees(x, s, c);
    return c == 0 ? 0.0 : c;
}
//=============================================================================
// sin(a + bi) = sin(a)cosh(b) + i cos(a)sinh(b) in degrees; an exact zero factor stays
// zero even when the other factor is infinite.
template <class T, bool IsSine>
static inline std::complex<T>
degreesComplexValue(std::complex<T> z)
{
    double a = (double)z.real();
    if (z.imag() == 0) {
        return std::complex<T>((T)(IsSine ? sindValue(a) : cosdValue(a)), (T)0);
    }
    double s = sindValue(a);
    double c = cosdValue(a);
    double b = (double)z.imag() * (M_PI / 180.0);
    double chb = std::cosh(b);
    double shb = std::sinh(b);
    if (IsSine) {
        return std::complex<T>((T)(s == 0 ? s : s * chb), (T)(c == 0 ? c : c * shb));
    }
    return std::complex<T>((T)(c == 0 ? c : c * chb), (T)(s == 0 ? 0.0 : -s * shb));
}
//=============================================================================
template <bool IsSine> struct DegreesKernel
{
    static constexpr bool realOnly = true;
    template <class T>
    static bool
    inDomain(T)
    {
        return true;
    }
    template <class T>
    static T
    real(T x)
    {
        return (T)(IsSine ? sindValue((double)x) : cosdValue((double)x));
    }
    template <class T>
    static std::complex<T>
    realAsComplex(T x)
    {
        return std::complex<T>(real<T>(x), (T)0);
    }
    template <class T>
    static std::complex<T>
    complexValue(std::complex<T> z)
    {
        return degreesComplexValue<T, IsSine>(z);
    }
};
//=============================================================================
ArrayOf
Atanh(const ArrayOf& A, bool& needToOverload)
{
    return mapKernel<AtanhKernel>(A, needToOverload, doublecomplex(0., 0.));
}
//=============================================================================
ArrayOf
Asinh(const ArrayOf& A, bool& needToOverload)
{
    return mapKernel<AsinhKernel>(A, needToOverload, doublecomplex(0., 0.));
}
//=============================================================================
ArrayOf
Acosh(const ArrayOf& A, bool& needToOverload)
{
    return mapKernel<AcoshKernel>(A, needToOverload, doublecomplex(0., M_PI / 2.));
}
//=============================================================================
ArrayOf
Sind(const ArrayOf& A, bool& needToOverload)
{
    return mapKernel<DegreesKernel<true>>(A, needToOverload, doublecomplex(0., 0.));
}
//=============================================================================
ArrayOf
Cosd(const ArrayOf& A, bool& needToOverload)
{
    return mapKernel<DegreesKernel<false>>(A, needToOverload, doublecomplex(1., 0.));
}
//=============================================================================
} // namespace Nelson
//=============================================================================
