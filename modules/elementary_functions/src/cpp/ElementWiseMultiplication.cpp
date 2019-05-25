//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
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
#include "IntegerOperations.hpp"
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
integer_times(ArrayOf& A, ArrayOf& B)
{
    T* ptrA = (T*)A.getDataPointer();
    T* ptrB = (T*)B.getDataPointer();
    if (A.isScalar() && B.isScalar()) {
        // s .* s
        void* Cp = new_with_exception<T>(1);
        T* ptrC = (T*)Cp;
        ptrC[0] = scalarInteger_times_scalarInteger(ptrA[0], ptrB[0]);
        Dimensions dimsC(1, 1);
        return ArrayOf(A.getDataClass(), dimsC, Cp, false);
    } else {
        if (A.isScalar() || B.isScalar()) {
            // mxn .* s
            // s .* mxn
            Dimensions dimsC;
            void* Cp = nullptr;
            indexType Clen;
            if (A.isScalar()) {
                T* ptrA = (T*)A.getDataPointer();
                dimsC = B.getDimensions();
                Clen = dimsC.getElementCount();
                Cp = new_with_exception<T>(Clen);
                T* ptrC = (T*)Cp;
                for (indexType k = 0; k < B.getDimensions().getElementCount(); k++) {
                    ptrC[k] = scalarInteger_times_scalarInteger(ptrA[0], ptrB[k]);
                }
            } else {
                T* ptrB = (T*)B.getDataPointer();
                dimsC = A.getDimensions();
                Clen = dimsC.getElementCount();
                Cp = new_with_exception<T>(Clen);
                T* ptrC = (T*)Cp;
                for (indexType k = 0; k < A.getDimensions().getElementCount(); k++) {
                    ptrC[k] = scalarInteger_times_scalarInteger(ptrA[k], ptrB[0]);
                }
            }
            return ArrayOf(A.getDataClass(), dimsC, Cp, false);
        } else {
            // mxn .* mxn
            Dimensions dimsC = A.getDimensions();
            if (A.isEmpty(true)) {
                ArrayOf res = ArrayOf::emptyConstructor(dimsC);
                res.promoteType(A.getDataClass());
                return res;
            } else {
                indexType Clen = dimsC.getElementCount();
                void* Cp = new_with_exception<T>(Clen);
                T* ptrC = (T*)Cp;
                for (indexType k = 0; k < A.getDimensions().getElementCount(); k++) {
                    ptrC[k] = scalarInteger_times_scalarInteger(ptrA[k], ptrB[k]);
                }
                return ArrayOf(A.getDataClass(), dimsC, Cp, false);
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
        Error(_W("Size mismatch on arguments to arithmetic operator ") + L".*");
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
template <class T>
ArrayOf
integer_times_integer(ArrayOf& A, ArrayOf& B)
{
    Dimensions dimsA = A.getDimensions();
    Dimensions dimsB = B.getDimensions();
    if (!(SameSizeCheck(dimsA, dimsB) || A.isScalar() || B.isScalar())) {
        Error(_W("Size mismatch on arguments to arithmetic operator ") + L".*");
    }
    return integer_times<T>(A, B);
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
    } else if (A.isSingleClass() && B.isDoubleClass()) {
        return T_times_T<single>(NLS_SINGLE, NLS_SCOMPLEX, A, B);
    } else if (A.isDoubleClass() && B.isSingleClass()) {
        return T_times_T<single>(NLS_SINGLE, NLS_SCOMPLEX, A, B);
    } else {
        bool isIntegerA = A.isIntegerType() || A.isNdArrayIntegerType();
        bool isIntegerB = B.isIntegerType() || B.isNdArrayIntegerType();
        if (isIntegerA && (B.isDoubleType() && B.isScalar())) {
            if (B.isComplex()) {
                Error(_W("Complex integer not allowed for arithmetic operator ") + L".*");
            }
            B.promoteType(A.getDataClass());
            return elementWiseMultiplication(A, B, needToOverload);
        } else if (isIntegerB && (A.isDoubleType() && A.isScalar())) {
            if (A.isComplex()) {
                Error(_W("Complex integer not allowed for arithmetic operator ") + L".*");
            }
            A.promoteType(B.getDataClass());
            return elementWiseMultiplication(A, B, needToOverload);
        } else if (isIntegerA && isIntegerB) {
            if (A.getDataClass() != B.getDataClass()) {
                needToOverload = true;
            } else {
                switch (A.getDataClass()) {
                case NLS_UINT8:
                    return integer_times_integer<uint8>(A, B);
                case NLS_INT8:
                    return integer_times_integer<int8>(A, B);
                case NLS_UINT16:
                    return integer_times_integer<uint16>(A, B);
                case NLS_INT16:
                    return integer_times_integer<int16>(A, B);
                case NLS_UINT32:
                    return integer_times_integer<uint32>(A, B);
                case NLS_INT32:
                    return integer_times_integer<int32>(A, B);
                case NLS_UINT64:
                    return integer_times_integer<uint32>(A, B);
                case NLS_INT64:
                    return integer_times_integer<int32>(A, B);
                default:
                    needToOverload = true;
                    break;
                }
            }
        } else {
            needToOverload = true;
        }
    }
    return ArrayOf();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
