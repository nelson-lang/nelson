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
#include <Eigen/Dense>
#include "ElementWiseMultiplication.hpp"
#include "MatrixCheck.hpp"
#include "Error.hpp"
#include "Exception.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
static ArrayOf
real_times(Class currentClass, ArrayOf& A, ArrayOf& B)
{
    if (A.isScalar() && B.isScalar()) {
        // s .* s
        if (currentClass == NLS_DOUBLE) {
            return ArrayOf::doubleConstructor(
                A.getContentAsDoubleScalar() * B.getContentAsDoubleScalar());
        } else {
            return ArrayOf::singleConstructor(
                A.getContentAsSingleScalar() * B.getContentAsSingleScalar());
        }
    } else {
        if (A.isScalar() || B.isScalar()) {
            // mxn .* s
            // s .* mxn
            Dimensions dimsC;
            void* Cp = nullptr;
            indexType Clen;
            if (A.isScalar()) {
                T da;
                if (currentClass == NLS_DOUBLE) {
                    da = (T)A.getContentAsDoubleScalar();
                } else {
                    da = (T)A.getContentAsSingleScalar();
                }
                dimsC = B.getDimensions();
                Clen = dimsC.getElementCount();
                Cp = new_with_exception<T>(Clen);
                Eigen::Map<Eigen::Matrix<T, -1, -1>> matC((T*)Cp, 1, Clen);
                Eigen::Map<Eigen::Matrix<T, -1, -1>> matB((T*)B.getDataPointer(), 1, Clen);
                matC = da * matB.array();
            } else {
                T db;
                if (currentClass == NLS_DOUBLE) {
                    db = (T)B.getContentAsDoubleScalar();
                } else {
                    db = (T)B.getContentAsSingleScalar();
                }
                dimsC = A.getDimensions();
                Clen = dimsC.getElementCount();
                Cp = new_with_exception<T>(Clen);
                Eigen::Map<Eigen::Matrix<T, -1, -1>> matC((T*)Cp, 1, Clen);
                Eigen::Map<Eigen::Matrix<T, -1, -1>> matA((T*)A.getDataPointer(), 1, Clen);
                matC = matA.array() * db;
            }
            return ArrayOf(currentClass, dimsC, Cp, false);
        } else {
            // mxn .* mxn
            Dimensions dimsC = A.getDimensions();
            if (A.isEmpty(true)) {
                return ArrayOf::emptyConstructor(dimsC);
            } else {
                indexType Clen = dimsC.getElementCount();
                void* Cp = new_with_exception<T>(Clen);
                Eigen::Map<Eigen::Matrix<T, -1, -1>> matC((T*)Cp, 1, Clen);
                Eigen::Map<Eigen::Matrix<T, -1, -1>> matA((T*)A.getDataPointer(), 1, Clen);
                Eigen::Map<Eigen::Matrix<T, -1, -1>> matB((T*)B.getDataPointer(), 1, Clen);
                matC = matA.cwiseProduct(matB);
                return ArrayOf(currentClass, dimsC, Cp, false);
            }
        }
    }
    return ArrayOf();
}
//=============================================================================
template <class T>
static ArrayOf
complex_times(Class currentClass, ArrayOf& A, ArrayOf& B)
{
    ArrayOf res;
    A.promoteType(currentClass);
    B.promoteType(currentClass);
    if (A.isScalar() && B.isScalar()) {
        // s .* s
        void* Cp = new_with_exception<T>(2);
        std::complex<T>* Cz = reinterpret_cast<std::complex<T>*>(Cp);
        std::complex<T>* Az = reinterpret_cast<std::complex<T>*>((T*)A.getDataPointer());
        std::complex<T> cxa = Az[0];
        std::complex<T>* Bz = reinterpret_cast<std::complex<T>*>((T*)B.getDataPointer());
        std::complex<T> cxb = Bz[0];
        Cz[0] = cxa * cxb;
        res = ArrayOf(currentClass, A.getDimensions(), Cp, false);
    } else {
        if (A.isScalar() || B.isScalar()) {
            // mxn .* s
            // s .* mxn
            // s .* s
            Dimensions dimsC;
            void* Cp = nullptr;
            if (A.isScalar()) {
                dimsC = B.getDimensions();
                Cp = new_with_exception<T>(2 * dimsC.getElementCount());
                std::complex<T>* Cz = reinterpret_cast<std::complex<T>*>(Cp);
                Eigen::Map<Eigen::Matrix<std::complex<T>, -1, -1>> matC(
                    Cz, 1, dimsC.getElementCount());
                std::complex<T>* Az = reinterpret_cast<std::complex<T>*>((T*)A.getDataPointer());
                std::complex<T>* Bz = reinterpret_cast<std::complex<T>*>((T*)B.getDataPointer());
                Eigen::Map<Eigen::Matrix<std::complex<T>, -1, -1>> matB(
                    Bz, 1, dimsC.getElementCount());
                matC = Az[0] * matB.array();
            } else {
                dimsC = A.getDimensions();
                Cp = new_with_exception<T>(2 * dimsC.getElementCount());
                std::complex<T>* Cz = reinterpret_cast<std::complex<T>*>(Cp);
                Eigen::Map<Eigen::Matrix<std::complex<T>, -1, -1>> matC(
                    Cz, 1, dimsC.getElementCount());
                std::complex<T>* Bz = reinterpret_cast<std::complex<T>*>((T*)B.getDataPointer());
                std::complex<T>* Az = reinterpret_cast<std::complex<T>*>((T*)A.getDataPointer());
                Eigen::Map<Eigen::Matrix<std::complex<T>, -1, -1>> matA(
                    Az, 1, dimsC.getElementCount());
                matC = matA.array() * Bz[0];
            }
            res = ArrayOf(currentClass, dimsC, Cp, false);
        } else {
            // mxn .* mxn
            Dimensions dimsC = A.getDimensions();
            if (A.isEmpty(true)) {
                return ArrayOf::emptyConstructor(dimsC);
            } else {
                void* Cp = new_with_exception<T>(2 * dimsC.getElementCount());
                std::complex<T>* Cz = reinterpret_cast<std::complex<T>*>(Cp);
                std::complex<T>* Az = reinterpret_cast<std::complex<T>*>((T*)A.getDataPointer());
                Eigen::Map<Eigen::Matrix<std::complex<T>, -1, -1>> matA(
                    Az, 1, dimsC.getElementCount());
                std::complex<T>* Bz = reinterpret_cast<std::complex<T>*>((T*)B.getDataPointer());
                Eigen::Map<Eigen::Matrix<std::complex<T>, -1, -1>> matB(
                    Bz, 1, dimsC.getElementCount());
                Eigen::Map<Eigen::Matrix<std::complex<T>, -1, -1>> matC(
                    Cz, 1, dimsC.getElementCount());
                matC = matA.cwiseProduct(matB);
                res = ArrayOf(currentClass, dimsC, Cp, false);
            }
        }
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
T_times_T(Class realClass, Class complexClass, ArrayOf& A, ArrayOf& B)
{
    Dimensions dimsA = A.getDimensions();
    Dimensions dimsB = B.getDimensions();
    if (!(SameSizeCheck(dimsA, dimsB) || A.isScalar() || B.isScalar())) {
        Error(_W("Size mismatch on arguments to arithmetic operator ") + L"*");
    }
    if (A.isComplex() || B.isComplex()) {
        ArrayOf res = complex_times<T>(complexClass, A, B);
        if (res.allReal()) {
            res.promoteType(realClass);
        }
        return res;
    }
    return real_times<T>(realClass, A, B);
}
//=============================================================================
ArrayOf
elementWiseMultiplication(ArrayOf& A, ArrayOf& B, bool& needToOverload)
{
    if (A.isSparse() || B.isSparse()) {
        needToOverload = true;
    } else if (A.isDoubleClass() && B.isDoubleClass()) {
        return T_times_T<double>(NLS_DOUBLE, NLS_DCOMPLEX, A, B);
    } else if (A.isSingleClass() && B.isSingleClass()) {
        return T_times_T<single>(NLS_SINGLE, NLS_SCOMPLEX, A, B);
    } else if (A.getDataClass() == B.getDataClass()) {
        needToOverload = true;
    } else {
        needToOverload = true;
    }
    return ArrayOf();
}
//=============================================================================
}
//=============================================================================
