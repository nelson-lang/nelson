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
#include <complex>
#include <Eigen/Dense>
#include "Exponential.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
T
ExponentialComplexScalar(T value)
{
    // Z = X + i * Y
    // EXP(Z) = EXP(X) * (COS(Y) + i * SIN(Y))
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
ArrayOf
Exponential(ArrayOf A, bool& needToOverload)
{
    ArrayOf res;
    needToOverload = false;
    Class classA = A.getDataClass();
    if (classA == NLS_DOUBLE || classA == NLS_DCOMPLEX) {
        if (A.isSparse()) {
            needToOverload = true;
        } else {
            Dimensions dimsA = A.getDimensions();
            if (classA == NLS_DOUBLE) {
                double* ptrOut = (double*)ArrayOf::allocateArrayOf(
                    NLS_DOUBLE, dimsA.getElementCount(), stringVector(), false);
                double* ptrIn = (double*)A.getDataPointer();
                Eigen::Map<Eigen::ArrayXd> matOut(ptrOut, dimsA.getElementCount());
                Eigen::Map<Eigen::ArrayXd> matIn(ptrIn, dimsA.getElementCount());
                matOut = matIn.unaryExpr(std::ref(ExponentialRealScalar<double>));
                res = ArrayOf(NLS_DOUBLE, dimsA, ptrOut);
            } else {
                double* ptrOut = (double*)ArrayOf::allocateArrayOf(
                    NLS_DCOMPLEX, dimsA.getElementCount(), stringVector(), false);
                std::complex<double>* Cz = reinterpret_cast<std::complex<double>*>((double*)ptrOut);
                double* ptrIn = (double*)A.getDataPointer();
                std::complex<double>* Az
                    = reinterpret_cast<std::complex<double>*>((double*)A.getDataPointer());
                Eigen::Map<Eigen::ArrayXcd> matOut(Cz, 1, dimsA.getElementCount());
                Eigen::Map<Eigen::ArrayXcd> matIn(Az, 1, dimsA.getElementCount());
                for (indexType k = 0; k < dimsA.getElementCount(); k++) {
                    matOut[k] = ExponentialComplexScalar<std::complex<double>>(matIn[k]);
                }
                res = ArrayOf(NLS_DCOMPLEX, dimsA, ptrOut);
            }
        }
    } else if (classA == NLS_SINGLE || classA == NLS_SCOMPLEX) {
        if (A.isSparse()) {
            needToOverload = true;
        } else {
            Dimensions dimsA = A.getDimensions();
            if (classA == NLS_SINGLE) {
                single* ptrOut = (single*)ArrayOf::allocateArrayOf(
                    NLS_SINGLE, dimsA.getElementCount(), stringVector(), false);
                single* ptrIn = (single*)A.getDataPointer();
                Eigen::Map<Eigen::ArrayXf> matOut(ptrOut, dimsA.getElementCount());
                Eigen::Map<Eigen::ArrayXf> matIn(ptrIn, dimsA.getElementCount());
                matOut = matIn.unaryExpr(std::ref(ExponentialRealScalar<single>));
                res = ArrayOf(NLS_SINGLE, dimsA, ptrOut);
            } else {
                single* ptrOut = (single*)ArrayOf::allocateArrayOf(
                    NLS_SCOMPLEX, dimsA.getElementCount(), stringVector(), false);
                std::complex<single>* Cz = reinterpret_cast<std::complex<single>*>((single*)ptrOut);
                single* ptrIn = (single*)A.getDataPointer();
                std::complex<single>* Az
                    = reinterpret_cast<std::complex<single>*>((single*)A.getDataPointer());
                Eigen::Map<Eigen::ArrayXcf> matOut(Cz, 1, dimsA.getElementCount());
                Eigen::Map<Eigen::ArrayXcf> matIn(Az, 1, dimsA.getElementCount());
                for (indexType k = 0; k < dimsA.getElementCount(); k++) {
                    matOut[k] = ExponentialComplexScalar<std::complex<single>>(matIn[k]);
                }
                res = ArrayOf(NLS_SCOMPLEX, dimsA, ptrOut);
            }
        }
    } else {
        needToOverload = true;
    }
    return res;
}
//=============================================================================
}
//=============================================================================
