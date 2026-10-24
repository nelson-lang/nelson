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
#include "lapack_eigen_config.hpp"
#include <Eigen/Dense>
#include <algorithm>
#include <cmath>
#include <limits>
#include "Norm.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "SparseType.hpp"
#include <Eigen/Sparse>
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
static double
sparseAbsValue(const T& value)
{
    return std::abs(value);
}
//=============================================================================
static ArrayOf
sparseNormScalar(double value, NelsonType dataClass)
{
    if (dataClass == NLS_SINGLE || dataClass == NLS_SCOMPLEX) {
        return ArrayOf::singleScalar(static_cast<single>(value));
    }
    return ArrayOf::doubleScalar(value);
}
//=============================================================================
template <class T>
static ArrayOf
SparseNormTemplate(const ArrayOf& arrayIn, double p, bool frobenius)
{
    const auto* spmatSRC = static_cast<const Eigen::SparseMatrix<T, 0, signedIndexType>*>(
        arrayIn.sparseDataPointer());
    double result = 0;
    if (frobenius || arrayIn.isVector() || arrayIn.isScalar()) {
        if (std::isinf(p) && p > 0 && !frobenius) {
            for (signedIndexType col = 0; col < spmatSRC->outerSize(); ++col) {
                for (typename Eigen::SparseMatrix<T, 0, signedIndexType>::InnerIterator it(
                         *spmatSRC, col);
                    it; ++it) {
                    result = std::max(result, sparseAbsValue<T>(it.value()));
                }
            }
        } else {
            double accum = 0;
            double exponent = frobenius ? 2 : p;
            for (signedIndexType col = 0; col < spmatSRC->outerSize(); ++col) {
                for (typename Eigen::SparseMatrix<T, 0, signedIndexType>::InnerIterator it(
                         *spmatSRC, col);
                    it; ++it) {
                    accum += std::pow(sparseAbsValue<T>(it.value()), exponent);
                }
            }
            result = std::pow(accum, 1.0 / exponent);
        }
        return sparseNormScalar(result, arrayIn.dataClass());
    }

    if (p == 1) {
        for (signedIndexType col = 0; col < spmatSRC->outerSize(); ++col) {
            double accum = 0;
            for (typename Eigen::SparseMatrix<T, 0, signedIndexType>::InnerIterator it(
                     *spmatSRC, col);
                it; ++it) {
                accum += sparseAbsValue<T>(it.value());
            }
            result = std::max(result, accum);
        }
        return sparseNormScalar(result, arrayIn.dataClass());
    }
    if (std::isinf(p) && p > 0) {
        std::vector<double> rowSums(arrayIn.rows(), 0);
        for (signedIndexType col = 0; col < spmatSRC->outerSize(); ++col) {
            for (typename Eigen::SparseMatrix<T, 0, signedIndexType>::InnerIterator it(
                     *spmatSRC, col);
                it; ++it) {
                rowSums[it.row()] += sparseAbsValue<T>(it.value());
            }
        }
        for (double rowSum : rowSums) {
            result = std::max(result, rowSum);
        }
        return sparseNormScalar(result, arrayIn.dataClass());
    }
    Error(L"Nelson:invalidValue", L"Wrong value for #%d argument.", (2));
    return {};
}
//=============================================================================
static ArrayOf
SparseNorm(const ArrayOf& arrayIn, double p, bool frobenius)
{
    if (!arrayIn.is2D()) {
        Error(L"Nelson:expected2D", L"%s must be 2-D.", L"Argument #1");
    }
    if (frobenius) {
        p = 2;
    }
    switch (arrayIn.dataClass()) {
    case NLS_SINGLE:
        return SparseNormTemplate<single>(arrayIn, p, frobenius);
    case NLS_SCOMPLEX:
        return SparseNormTemplate<singlecomplex>(arrayIn, p, frobenius);
    case NLS_DOUBLE:
        return SparseNormTemplate<double>(arrayIn, p, frobenius);
    case NLS_DCOMPLEX:
        return SparseNormTemplate<doublecomplex>(arrayIn, p, frobenius);
    default:
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
            L"single or double");
    }
    return {};
}
//=============================================================================
static bool
ispNormValid(double p)
{
    return (p == 1 || p == 2 || std::isinf(p) && (p > 0));
}
//=============================================================================
// p-norm of a vector from the magnitudes of its elements.
// NaN propagates; the sum of the powers is scaled by the largest magnitude so
// that neither overflow nor underflow spoils the result (the plain 2-norm is
// kept whenever it is representable, so ordinary inputs are unchanged).
template <class T, class MagnitudeAt>
static T
magnitudeNorm(indexType n, MagnitudeAt magnitudeAt, double p)
{
    const T infinity = std::numeric_limits<T>::infinity();
    if (p == 2) {
        T sumOfSquares = 0;
        for (indexType k = 0; k < n; ++k) {
            T a = magnitudeAt(k);
            sumOfSquares += a * a;
        }
        T plain = std::sqrt(sumOfSquares);
        if (std::isnan(plain)
            || (std::isfinite(plain) && plain >= std::sqrt(std::numeric_limits<T>::min()))) {
            return plain;
        }
    } else if (p == 1) {
        T sum = 0;
        for (indexType k = 0; k < n; ++k) {
            sum += magnitudeAt(k);
        }
        return sum;
    }
    T maxAbs = 0;
    T minAbs = infinity;
    for (indexType k = 0; k < n; ++k) {
        T a = magnitudeAt(k);
        if (std::isnan(a)) {
            return std::numeric_limits<T>::quiet_NaN();
        }
        maxAbs = std::max(maxAbs, a);
        minAbs = std::min(minAbs, a);
    }
    if (std::isinf(p)) {
        return (p > 0) ? maxAbs : minAbs;
    }
    if (p == 0) {
        // sum(|x|^0)^(1/0): every magnitude contributes one
        return std::pow(static_cast<T>(n), infinity);
    }
    if (maxAbs == 0) {
        return 0;
    }
    T exponent = static_cast<T>(p);
    if (std::isinf(maxAbs)) {
        T sum = 0;
        for (indexType k = 0; k < n; ++k) {
            sum += std::pow(magnitudeAt(k), exponent);
        }
        return std::pow(sum, T(1) / exponent);
    }
    T sum = 0;
    for (indexType k = 0; k < n; ++k) {
        sum += std::pow(magnitudeAt(k) / maxAbs, exponent);
    }
    return maxAbs * std::pow(sum, T(1) / exponent);
}
//=============================================================================
// vectorised p-norm. Returns false when the result must be recomputed by
// magnitudeNorm: an empty vector, or a sum of powers that overflowed, underflowed
// or is NaN (so ordinary inputs never leave this fast path).
template <class Array>
static bool
fastVectorNorm(const Array& values, double p, typename Array::RealScalar& result)
{
    using Real = typename Array::RealScalar;
    if (values.size() == 0) {
        return false;
    }
    if (p == 1) {
        result = values.abs().sum();
        return true;
    }
    if (std::isinf(p)) {
        result = (p > 0) ? values.abs().template maxCoeff<Eigen::PropagateNaN>()
                         : values.abs().template minCoeff<Eigen::PropagateNaN>();
        return true;
    }
    const Real lowest = std::numeric_limits<Real>::min() * static_cast<Real>(9007199254740992.0);
    Real sum;
    if (p == 2) {
        sum = values.abs2().sum();
        if (std::isfinite(sum) && sum >= lowest) {
            result = std::sqrt(sum);
            return true;
        }
        return false;
    }
    if (p <= 0) {
        return false;
    }
    Real exponent = static_cast<Real>(p);
    sum = values.abs().pow(exponent).sum();
    if (std::isfinite(sum) && sum >= lowest) {
        result = std::pow(sum, Real(1) / exponent);
        return std::isfinite(result);
    }
    return false;
}
//=============================================================================
template <class T>
T
NormPVector(const ArrayOf& arrayIn, double p)
{
    const T* values = (const T*)arrayIn.dataPointer();
    Eigen::Map<const Eigen::Array<T, Eigen::Dynamic, 1>> mapped(
        values, (Eigen::Index)arrayIn.elementCount());
    T result;
    if (fastVectorNorm(mapped, p, result)) {
        return result;
    }
    return magnitudeNorm<T>(
        arrayIn.elementCount(), [values](indexType k) { return std::abs(values[k]); }, p);
}
//=============================================================================
template <class T>
T
NormPComplexVector(const ArrayOf& arrayIn, double p)
{
    const std::complex<T>* values
        = reinterpret_cast<const std::complex<T>*>((const T*)arrayIn.dataPointer());
    Eigen::Map<const Eigen::Array<std::complex<T>, Eigen::Dynamic, 1>> mapped(
        values, (Eigen::Index)arrayIn.elementCount());
    T result;
    if (fastVectorNorm(mapped, p, result)) {
        return result;
    }
    return magnitudeNorm<T>(
        arrayIn.elementCount(), [values](indexType k) { return std::abs(values[k]); }, p);
}
//=============================================================================
template <class T>
T
NormP2Matrix(const ArrayOf& arrayIn)
{
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matArrayIn(
        (T*)arrayIn.dataPointer(), (Eigen::Index)arrayIn.rows(), (Eigen::Index)arrayIn.columns());
    bool allFinite = matArrayIn.allFinite();
    T res = (T)std::nan("");
    if (!allFinite) {
        bool hasNaN = matArrayIn.hasNaN();
        if (!hasNaN) {
            return std::numeric_limits<T>::infinity();
        }
    } else {
        Eigen::JacobiSVD<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> svd(matArrayIn);
        res = svd.singularValues()(0, 0);
        if (std::isnan(res) && !matArrayIn.hasNaN()) {
            res = std::numeric_limits<T>::infinity();
        }
    }
    return res;
}
//=============================================================================
template <class T>
T
NormP2ComplexMatrix(const ArrayOf& arrayIn)
{
    std::complex<T>* arrayInz = reinterpret_cast<std::complex<T>*>((T*)arrayIn.dataPointer());
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matArrayIn(
        arrayInz, (Eigen::Index)arrayIn.rows(), (Eigen::Index)arrayIn.columns());
    Eigen::JacobiSVD<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> svd(
        matArrayIn);
    T res = svd.singularValues()(0, 0);
    if (std::isnan(res)) {
        if (!matArrayIn.allFinite() && !matArrayIn.hasNaN()) {
            res = std::numeric_limits<T>::infinity();
        }
    }
    return res;
}
//=============================================================================
template <class T>
T
NormP1ComplexMatrix(const ArrayOf& arrayIn)
{
    std::complex<T>* arrayInz = reinterpret_cast<std::complex<T>*>((T*)arrayIn.dataPointer());
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matArrayIn(
        arrayInz, (Eigen::Index)arrayIn.rows(), (Eigen::Index)arrayIn.columns());
    if (arrayIn.isScalar() || arrayIn.isVector()) {
        T res = matArrayIn.template lpNorm<1>();
        return res;
    }
    T maxValue = 0.0;
    for (Eigen::Index i = 0; i < matArrayIn.cols(); ++i) {
        T cn = matArrayIn.col(i).template lpNorm<1>();
        if (std::isnan(cn)) {
            return cn;
        }
        if (cn > maxValue) {
            maxValue = cn;
        }
    }
    return maxValue;
}
//=============================================================================
template <class T>
T
NormP1Matrix(const ArrayOf& arrayIn)
{
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matArrayIn(
        (T*)arrayIn.dataPointer(), (Eigen::Index)arrayIn.rows(), (Eigen::Index)arrayIn.columns());
    if (arrayIn.isScalar() || arrayIn.isVector()) {
        T res = matArrayIn.template lpNorm<1>();
        return res;
    }
    T maxValue = 0.0;
    for (Eigen::Index i = 0; i < matArrayIn.cols(); ++i) {
        T cn = matArrayIn.col(i).template lpNorm<1>();
        if (std::isnan(cn)) {
            return cn;
        }
        if (cn > maxValue) {
            maxValue = cn;
        }
    }
    return maxValue;
}
//=============================================================================
template <class T>
T
NormPInfComplexMatrix(const ArrayOf& arrayIn)
{
    std::complex<T>* arrayInz = reinterpret_cast<std::complex<T>*>((T*)arrayIn.dataPointer());
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matArrayIn(
        arrayInz, (Eigen::Index)arrayIn.rows(), (Eigen::Index)arrayIn.columns());
    if (arrayIn.isScalar() || arrayIn.isVector()) {
        return matArrayIn.template lpNorm<Eigen::Infinity>();
    }
    T maxValue = 0.0;
    if (!matArrayIn.hasNaN()) {
        if (matArrayIn.allFinite()) {
            for (int i = 0; i < matArrayIn.rows(); ++i) {
                T cn = matArrayIn.row(i).template lpNorm<1>();
                if (cn > maxValue) {
                    maxValue = cn;
                }
            }
        } else {
            maxValue = std::numeric_limits<T>::infinity();
        }
    } else {
        maxValue = (T)std::nan("");
    }
    return maxValue;
}
//=============================================================================
template <class T>
T
NormPInfMatrix(const ArrayOf& arrayIn)
{
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matArrayIn(
        (T*)arrayIn.dataPointer(), (Eigen::Index)arrayIn.rows(), (Eigen::Index)arrayIn.columns());
    if (arrayIn.isScalar() || arrayIn.isVector()) {
        return matArrayIn.template lpNorm<Eigen::Infinity>();
    }
    T maxValue = 0.0;
    if (!matArrayIn.hasNaN()) {
        if (matArrayIn.allFinite()) {
            for (int i = 0; i < matArrayIn.rows(); ++i) {
                T cn = matArrayIn.row(i).template lpNorm<1>();
                if (cn > maxValue) {
                    maxValue = cn;
                }
            }
        } else {
            maxValue = std::numeric_limits<T>::infinity();
        }
    } else {
        maxValue = (T)std::nan("");
    }
    return maxValue;
}
//=============================================================================
template <class T>
T
NormFrobeniusComplexMatrix(const ArrayOf& arrayIn)
{
    return NormPComplexVector<T>(arrayIn, 2);
}
//=============================================================================
template <class T>
T
NormFrobeniusMatrix(const ArrayOf& arrayIn)
{
    return NormPVector<T>(arrayIn, 2);
}
//=============================================================================
ArrayOf
Norm(const ArrayOf& arrayIn, double p)
{
    ArrayOf res;
    if (arrayIn.isSparse()) {
        return SparseNorm(arrayIn, p, false);
    }
    if (!arrayIn.is2D()) {
        Error(L"Nelson:expected2D", L"%s must be 2-D.", L"Argument #1");
    }

    if (!ispNormValid(p) && !(arrayIn.isVector() || arrayIn.isScalar())) {
        Error(L"Nelson:invalidValue", L"Wrong value for #%d argument.", (2));
    }
    double normResultAsDouble = 0;
    single normResultAsSingle = 0;
    if (arrayIn.isEmpty()) {
        if (arrayIn.dataClass() == NLS_SCOMPLEX || arrayIn.dataClass() == NLS_SINGLE) {
            res = ArrayOf::singleScalar(normResultAsSingle);
        } else {
            res = ArrayOf::doubleScalar(normResultAsDouble);
        }
    } else {
        if (arrayIn.isVector() || arrayIn.isScalar()) {
            if (arrayIn.dataClass() == NLS_SCOMPLEX) {
                normResultAsSingle = NormPComplexVector<single>(arrayIn, p);
                res = ArrayOf::singleScalar(normResultAsSingle);
            }
            if (arrayIn.dataClass() == NLS_SINGLE) {
                normResultAsSingle = NormPVector<single>(arrayIn, p);
                res = ArrayOf::singleScalar(normResultAsSingle);
            }
            if (arrayIn.dataClass() == NLS_DCOMPLEX) {
                normResultAsDouble = NormPComplexVector<double>(arrayIn, p);
                res = ArrayOf::doubleScalar(normResultAsDouble);
            }
            if (arrayIn.dataClass() == NLS_DOUBLE) {
                normResultAsDouble = NormPVector<double>(arrayIn, p);
                res = ArrayOf::doubleScalar(normResultAsDouble);
            }
        } else {
            if (p == 2) {
                if (arrayIn.dataClass() == NLS_SCOMPLEX) {
                    normResultAsSingle = NormP2ComplexMatrix<single>(arrayIn);
                    res = ArrayOf::singleScalar(normResultAsSingle);
                }
                if (arrayIn.dataClass() == NLS_SINGLE) {
                    normResultAsSingle = NormP2Matrix<single>(arrayIn);
                    res = ArrayOf::singleScalar(normResultAsSingle);
                }
                if (arrayIn.dataClass() == NLS_DCOMPLEX) {
                    normResultAsDouble = NormP2ComplexMatrix<double>(arrayIn);
                    res = ArrayOf::doubleScalar(normResultAsDouble);
                }
                if (arrayIn.dataClass() == NLS_DOUBLE) {
                    normResultAsDouble = NormP2Matrix<double>(arrayIn);
                    res = ArrayOf::doubleScalar(normResultAsDouble);
                }
            } else if (p == 1) {
                if (arrayIn.dataClass() == NLS_SCOMPLEX) {
                    normResultAsSingle = NormP1ComplexMatrix<single>(arrayIn);
                    res = ArrayOf::singleScalar(normResultAsSingle);
                }
                if (arrayIn.dataClass() == NLS_SINGLE) {
                    normResultAsSingle = NormP1Matrix<single>(arrayIn);
                    res = ArrayOf::singleScalar(normResultAsSingle);
                }
                if (arrayIn.dataClass() == NLS_DCOMPLEX) {
                    normResultAsDouble = NormP1ComplexMatrix<double>(arrayIn);
                    res = ArrayOf::doubleScalar(normResultAsDouble);
                }
                if (arrayIn.dataClass() == NLS_DOUBLE) {
                    normResultAsDouble = NormP1Matrix<double>(arrayIn);
                    res = ArrayOf::doubleScalar(normResultAsDouble);
                }
            } else if (std::isinf(p)) {
                if (arrayIn.dataClass() == NLS_SCOMPLEX) {
                    normResultAsSingle = NormPInfComplexMatrix<single>(arrayIn);
                    res = ArrayOf::singleScalar(normResultAsSingle);
                }
                if (arrayIn.dataClass() == NLS_SINGLE) {
                    normResultAsSingle = NormPInfMatrix<single>(arrayIn);
                    res = ArrayOf::singleScalar(normResultAsSingle);
                }
                if (arrayIn.dataClass() == NLS_DCOMPLEX) {
                    normResultAsDouble = NormPInfComplexMatrix<double>(arrayIn);
                    res = ArrayOf::doubleScalar(normResultAsDouble);
                }
                if (arrayIn.dataClass() == NLS_DOUBLE) {
                    normResultAsDouble = NormPInfMatrix<double>(arrayIn);
                    res = ArrayOf::doubleScalar(normResultAsDouble);
                }
            } else {
                Error(L"Nelson:invalidValue", L"Wrong value for #%d argument.", (2));
            }
        }
    }
    return res;
}
//=============================================================================
ArrayOf
NormFrobenius(const ArrayOf& arrayIn)
{
    ArrayOf res;
    if (arrayIn.isSparse()) {
        return SparseNorm(arrayIn, 2, true);
    }
    if (!arrayIn.is2D()) {
        Error(L"Nelson:expected2D", L"%s must be 2-D.", L"Argument #1");
    }
    double normResultAsDouble = 0;
    single normResultAsSingle = 0;
    if (arrayIn.isEmpty()) {
        if (arrayIn.dataClass() == NLS_SCOMPLEX || arrayIn.dataClass() == NLS_SINGLE) {
            res = ArrayOf::singleScalar(normResultAsSingle);
        } else {
            res = ArrayOf::doubleScalar(normResultAsDouble);
        }
    } else {
        if (arrayIn.dataClass() == NLS_SCOMPLEX) {
            normResultAsSingle = NormFrobeniusComplexMatrix<single>(arrayIn);
            res = ArrayOf::singleScalar(normResultAsSingle);
        }
        if (arrayIn.dataClass() == NLS_SINGLE) {
            normResultAsSingle = NormFrobeniusMatrix<single>(arrayIn);
            res = ArrayOf::singleScalar(normResultAsSingle);
        }
        if (arrayIn.dataClass() == NLS_DCOMPLEX) {
            normResultAsDouble = NormFrobeniusComplexMatrix<double>(arrayIn);
            res = ArrayOf::doubleScalar(normResultAsDouble);
        }
        if (arrayIn.dataClass() == NLS_DOUBLE) {
            normResultAsDouble = NormFrobeniusMatrix<double>(arrayIn);
            res = ArrayOf::doubleScalar(normResultAsDouble);
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
