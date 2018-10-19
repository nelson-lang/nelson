//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "lapack_eigen.hpp"
#include "Norm.hpp"
#include "Error.hpp"
#include <Eigen/Dense>
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
ispNormValid(double p)
{
    return (p == 1 || p == 2 || std::isinf(p) && (p > 0));
}
//=============================================================================
template <class T>
T
NormPVector(const ArrayOf& arrayIn, double p)
{
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matArrayIn(
        (T*)arrayIn.getDataPointer(), (Eigen::Index)arrayIn.getDimensions().getRows(),
        (Eigen::Index)arrayIn.getDimensions().getColumns());
    T returnedValue = 0;
    if (p == 1) {
        returnedValue = matArrayIn.template lpNorm<1>();
    } else if (p == 2) {
        returnedValue = matArrayIn.template lpNorm<2>();
    } else if (std::isinf(p)) {
        if (p > 0) {
            // max(abs(X))
            returnedValue = matArrayIn.array().abs().maxCoeff();
        } else {
            // min(abs(X))
            returnedValue = matArrayIn.array().abs().minCoeff();
        }
    } else {
        // sum(abs(X).^ p) ^ (1 / p)
        returnedValue = pow(matArrayIn.array().abs().pow(p).sum(), (1 / (T)p));
    }
    return returnedValue;
}
//=============================================================================
template <class T>
T
NormPComplexVector(const ArrayOf& arrayIn, double p)
{
    std::complex<T>* arrayInz = reinterpret_cast<std::complex<T>*>((T*)arrayIn.getDataPointer());
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matArrayIn(arrayInz,
        (Eigen::Index)arrayIn.getDimensions().getRows(),
        (Eigen::Index)arrayIn.getDimensions().getColumns());
    T returnedValue = 0;
    if (p == 1) {
        returnedValue = matArrayIn.template lpNorm<1>();
    } else if (p == 2) {
        returnedValue = matArrayIn.template lpNorm<2>();
    } else if (std::isinf(p)) {
        if (p > 0) {
            // max(abs(X))
            returnedValue = matArrayIn.array().abs().maxCoeff();
        } else {
            // min(abs(X))
            returnedValue = matArrayIn.array().abs().minCoeff();
        }
    } else {
        // sum(abs(X).^ p) ^ (1 / p)
        returnedValue = pow(matArrayIn.array().abs().pow(p).sum(), (1 / (T)p));
    }
    return returnedValue;
}
//=============================================================================
template <class T>
T
NormP2Matrix(const ArrayOf& arrayIn)
{
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matArrayIn(
        (T*)arrayIn.getDataPointer(), (Eigen::Index)arrayIn.getDimensions().getRows(),
        (Eigen::Index)arrayIn.getDimensions().getColumns());
    Eigen::JacobiSVD<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> svd(matArrayIn);
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
NormP2ComplexMatrix(const ArrayOf& arrayIn)
{
    std::complex<T>* arrayInz = reinterpret_cast<std::complex<T>*>((T*)arrayIn.getDataPointer());
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matArrayIn(arrayInz,
        (Eigen::Index)arrayIn.getDimensions().getRows(),
        (Eigen::Index)arrayIn.getDimensions().getColumns());
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
    std::complex<T>* arrayInz = reinterpret_cast<std::complex<T>*>((T*)arrayIn.getDataPointer());
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matArrayIn(arrayInz,
        (Eigen::Index)arrayIn.getDimensions().getRows(),
        (Eigen::Index)arrayIn.getDimensions().getColumns());
    if (arrayIn.isScalar() || arrayIn.isVector()) {
        T res = matArrayIn.template lpNorm<1>();
        return res;
    }
    T maxValue = 0.0;
    for (Eigen::Index i = 0; i < matArrayIn.cols(); ++i) {
        T cn = matArrayIn.col(i).template lpNorm<1>();
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
        (T*)arrayIn.getDataPointer(), (Eigen::Index)arrayIn.getDimensions().getRows(),
        (Eigen::Index)arrayIn.getDimensions().getColumns());
    if (arrayIn.isScalar() || arrayIn.isVector()) {
        T res = matArrayIn.template lpNorm<1>();
        return res;
    }
    T maxValue = 0.0;
    for (Eigen::Index i = 0; i < matArrayIn.cols(); ++i) {
        T cn = matArrayIn.col(i).template lpNorm<1>();
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
    std::complex<T>* arrayInz = reinterpret_cast<std::complex<T>*>((T*)arrayIn.getDataPointer());
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matArrayIn(arrayInz,
        (Eigen::Index)arrayIn.getDimensions().getRows(),
        (Eigen::Index)arrayIn.getDimensions().getColumns());
    if (arrayIn.isScalar() || arrayIn.isVector()) {
        return matArrayIn.template lpNorm<Eigen::Infinity>();
    }
    T maxValue = 0.0;
    if (!matArrayIn.hasNaN()) {
        if (matArrayIn.allFinite()) {
            for (int i = 0; i < matArrayIn.rows(); ++i) {
                T cn = matArrayIn.row(i).template lpNorm<1>();
                if (cn > maxValue)
                    maxValue = cn;
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
        (T*)arrayIn.getDataPointer(), (Eigen::Index)arrayIn.getDimensions().getRows(),
        (Eigen::Index)arrayIn.getDimensions().getColumns());
    if (arrayIn.isScalar() || arrayIn.isVector()) {
        return matArrayIn.template lpNorm<Eigen::Infinity>();
    }
    T maxValue = 0.0;
    if (!matArrayIn.hasNaN()) {
        if (matArrayIn.allFinite()) {
            for (int i = 0; i < matArrayIn.rows(); ++i) {
                T cn = matArrayIn.row(i).template lpNorm<1>();
                if (cn > maxValue)
                    maxValue = cn;
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
    std::complex<T>* arrayInz = reinterpret_cast<std::complex<T>*>((T*)arrayIn.getDataPointer());
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matArrayIn(arrayInz,
        (Eigen::Index)arrayIn.getDimensions().getRows(),
        (Eigen::Index)arrayIn.getDimensions().getColumns());
    return matArrayIn.norm();
}
//=============================================================================
template <class T>
T
NormFrobeniusMatrix(const ArrayOf& arrayIn)
{
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matArrayIn(
        (T*)arrayIn.getDataPointer(), (Eigen::Index)arrayIn.getDimensions().getRows(),
        (Eigen::Index)arrayIn.getDimensions().getColumns());
    return matArrayIn.norm();
}
//=============================================================================
ArrayOf
Norm(const ArrayOf& arrayIn, double p)
{
    ArrayOf res;
    if (!arrayIn.is2D()) {
        Error(ERROR_WRONG_ARGUMENT_1_SIZE_2D_MATRIX_EXPECTED);
    }

    if (!ispNormValid(p) && !(arrayIn.isVector() || arrayIn.isScalar())) {
        Error(ERROR_WRONG_ARGUMENT_2_VALUE);
    }
    double normResultAsDouble = 0;
    single normResultAsSingle = 0;
    if (arrayIn.isEmpty()) {
        if (arrayIn.getDataClass() == NLS_SCOMPLEX || arrayIn.getDataClass() == NLS_SINGLE) {
            res = ArrayOf::singleConstructor(normResultAsSingle);
        } else {
            res = ArrayOf::doubleConstructor(normResultAsDouble);
        }
    } else {
        if (arrayIn.isVector() || arrayIn.isScalar()) {
            if (arrayIn.getDataClass() == NLS_SCOMPLEX) {
                normResultAsSingle = NormPComplexVector<single>(arrayIn, p);
                res = ArrayOf::singleConstructor(normResultAsSingle);
            }
            if (arrayIn.getDataClass() == NLS_SINGLE) {
                normResultAsSingle = NormPVector<single>(arrayIn, p);
                res = ArrayOf::singleConstructor(normResultAsSingle);
            }
            if (arrayIn.getDataClass() == NLS_DCOMPLEX) {
                normResultAsDouble = NormPComplexVector<double>(arrayIn, p);
                res = ArrayOf::doubleConstructor(normResultAsDouble);
            }
            if (arrayIn.getDataClass() == NLS_DOUBLE) {
                normResultAsDouble = NormPVector<double>(arrayIn, p);
                res = ArrayOf::doubleConstructor(normResultAsDouble);
            }
        } else {
            // matrix
            if (p == 2) {
                if (arrayIn.getDataClass() == NLS_SCOMPLEX) {
                    normResultAsSingle = NormP2ComplexMatrix<single>(arrayIn);
                    res = ArrayOf::singleConstructor(normResultAsSingle);
                }
                if (arrayIn.getDataClass() == NLS_SINGLE) {
                    normResultAsSingle = NormP2Matrix<single>(arrayIn);
                    res = ArrayOf::singleConstructor(normResultAsSingle);
                }
                if (arrayIn.getDataClass() == NLS_DCOMPLEX) {
                    normResultAsDouble = NormP2ComplexMatrix<double>(arrayIn);
                    res = ArrayOf::doubleConstructor(normResultAsDouble);
                }
                if (arrayIn.getDataClass() == NLS_DOUBLE) {
                    normResultAsDouble = NormP2Matrix<double>(arrayIn);
                    res = ArrayOf::doubleConstructor(normResultAsDouble);
                }
            } else if (p == 1) {
                if (arrayIn.getDataClass() == NLS_SCOMPLEX) {
                    normResultAsSingle = NormP1ComplexMatrix<single>(arrayIn);
                    res = ArrayOf::singleConstructor(normResultAsSingle);
                }
                if (arrayIn.getDataClass() == NLS_SINGLE) {
                    normResultAsSingle = NormP1Matrix<single>(arrayIn);
                    res = ArrayOf::singleConstructor(normResultAsSingle);
                }
                if (arrayIn.getDataClass() == NLS_DCOMPLEX) {
                    normResultAsDouble = NormP1ComplexMatrix<double>(arrayIn);
                    res = ArrayOf::doubleConstructor(normResultAsDouble);
                }
                if (arrayIn.getDataClass() == NLS_DOUBLE) {
                    normResultAsDouble = NormP1Matrix<double>(arrayIn);
                    res = ArrayOf::doubleConstructor(normResultAsDouble);
                }
            } else if (std::isinf(p)) {
                if (arrayIn.getDataClass() == NLS_SCOMPLEX) {
                    normResultAsSingle = NormPInfComplexMatrix<single>(arrayIn);
                    res = ArrayOf::singleConstructor(normResultAsSingle);
                }
                if (arrayIn.getDataClass() == NLS_SINGLE) {
                    normResultAsSingle = NormPInfMatrix<single>(arrayIn);
                    res = ArrayOf::singleConstructor(normResultAsSingle);
                }
                if (arrayIn.getDataClass() == NLS_DCOMPLEX) {
                    normResultAsDouble = NormPInfComplexMatrix<double>(arrayIn);
                    res = ArrayOf::doubleConstructor(normResultAsDouble);
                }
                if (arrayIn.getDataClass() == NLS_DOUBLE) {
                    normResultAsDouble = NormPInfMatrix<double>(arrayIn);
                    res = ArrayOf::doubleConstructor(normResultAsDouble);
                }
            } else {
                Error(ERROR_WRONG_ARGUMENT_2_VALUE);
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
    if (!arrayIn.is2D()) {
        Error(ERROR_WRONG_ARGUMENT_1_SIZE_2D_MATRIX_EXPECTED);
    }
    double normResultAsDouble = 0;
    single normResultAsSingle = 0;
    if (arrayIn.isEmpty()) {
        if (arrayIn.getDataClass() == NLS_SCOMPLEX || arrayIn.getDataClass() == NLS_SINGLE) {
            res = ArrayOf::singleConstructor(normResultAsSingle);
        } else {
            res = ArrayOf::doubleConstructor(normResultAsDouble);
        }
    } else {
        if (arrayIn.getDataClass() == NLS_SCOMPLEX) {
            normResultAsSingle = NormFrobeniusComplexMatrix<single>(arrayIn);
            res = ArrayOf::singleConstructor(normResultAsSingle);
        }
        if (arrayIn.getDataClass() == NLS_SINGLE) {
            normResultAsSingle = NormFrobeniusMatrix<single>(arrayIn);
            res = ArrayOf::singleConstructor(normResultAsSingle);
        }
        if (arrayIn.getDataClass() == NLS_DCOMPLEX) {
            normResultAsDouble = NormFrobeniusComplexMatrix<double>(arrayIn);
            res = ArrayOf::doubleConstructor(normResultAsDouble);
        }
        if (arrayIn.getDataClass() == NLS_DOUBLE) {
            normResultAsDouble = NormFrobeniusMatrix<double>(arrayIn);
            res = ArrayOf::doubleConstructor(normResultAsDouble);
        }
    }
    return res;
}
//=============================================================================
}
//=============================================================================
