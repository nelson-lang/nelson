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
#include <Eigen/Dense>
#include "Substraction.hpp"
#include "MatrixCheck.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
empty_minus_generic(ArrayOf A, ArrayOf B, bool mustRaiseError, bool& bSuccess);
static Dimensions
getOutputDimensions(ArrayOf A, ArrayOf B);
static void
checkDimensions(ArrayOf A, ArrayOf B);
//=============================================================================
template <class T>
ArrayOf
real_minus_real(
    Class commonClass, const ArrayOf& A, const ArrayOf& B, bool mustRaiseError, bool& bSuccess)
{
    ArrayOf res;
    checkDimensions(A, B);
    if (A.isEmpty()) {
        return empty_minus_generic(A, B, mustRaiseError, bSuccess);
    }
    T* ptrA = (T*)A.getDataPointer();
    T* ptrB = (T*)B.getDataPointer();
    if (A.isScalar() && B.isScalar()) {
        T* ptrC = (T*)ArrayOf::allocateArrayOf(commonClass, 1);
        ptrC[0] = ptrA[0] - ptrB[0];
        res = ArrayOf(commonClass, Dimensions(1, 1), ptrC, false);
        bSuccess = true;
    } else {
        Dimensions dimsC = getOutputDimensions(A, B);
        indexType Clen = dimsC.getElementCount();
        T* ptrC = (T*)ArrayOf::allocateArrayOf(commonClass, Clen);
        Dimensions dimA = A.getDimensions();
        size_t mA = dimA.getRows();
        size_t nA = dimA.getColumns();
        Dimensions dimB = B.getDimensions();
        size_t mB = dimB.getRows();
        size_t nB = dimB.getColumns();
        if (A.isScalar()) {
            if (!B.is2D()) {
                for (size_t k = 0; k < Clen; k++) {
                    ptrC[k] = ptrA[0] - ptrB[k];
                }
            } else {
                size_t mC = dimsC.getRows();
                size_t nC = dimsC.getColumns();
                Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matC(ptrC, mC, nC);
                Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matB(ptrB, mB, nB);
                matC = ptrA[0] - matB.array();
            }
        } else if (B.isScalar()) {
            if (!A.is2D()) {
                for (size_t k = 0; k < Clen; k++) {
                    ptrC[k] = ptrA[k] - ptrB[0];
                }
            } else {
                size_t mC = dimsC.getRows();
                size_t nC = dimsC.getColumns();
                Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matC(ptrC, mC, nC);
                Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matA(ptrA, mA, nA);
                matC = matA.array() - ptrB[0];
            }
        } else {
            Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, 1>> matC(ptrC, dimsC.getElementCount(), 1);
            Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, 1>> matA(
                ptrA, A.getDimensions().getElementCount(), 1);
            Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, 1>> matB(
                ptrB, B.getDimensions().getElementCount(), 1);
            matC = matA - matB;
        }
        res = ArrayOf(commonClass, dimsC, ptrC, false);
        bSuccess = true;
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
complex_minus_complex(Class commonClassComplex, Class commonClassReal, const ArrayOf& A,
    const ArrayOf& B, bool mustRaiseError, bool& bSuccess)
{
    ArrayOf res;
    checkDimensions(A, B);
    if (A.isEmpty()) {
        return empty_minus_generic(A, B, mustRaiseError, bSuccess);
    }
    T* ptrA = (T*)A.getDataPointer();
    T* ptrB = (T*)B.getDataPointer();
    std::complex<T>* ptrAz = reinterpret_cast<std::complex<T>*>(ptrA);
    std::complex<T>* ptrBz = reinterpret_cast<std::complex<T>*>(ptrB);
    Dimensions dimsC = getOutputDimensions(A, B);
    if (A.isScalar() && B.isScalar()) {
        T* ptrC = (T*)ArrayOf::allocateArrayOf(commonClassComplex, 1);
        std::complex<T>* ptrCz = reinterpret_cast<std::complex<T>*>(ptrC);
        ptrCz[0] = ptrAz[0] - ptrBz[0];
        res = ArrayOf(commonClassComplex, Dimensions(1, 1), ptrC, false);
        bSuccess = true;
    } else {
        Dimensions dimsC = getOutputDimensions(A, B);
        indexType Clen = dimsC.getElementCount();
        T* ptrC = (T*)ArrayOf::allocateArrayOf(commonClassComplex, Clen);
        std::complex<T>* ptrCz = reinterpret_cast<std::complex<T>*>(ptrC);
        Dimensions dimA = A.getDimensions();
        size_t mA = dimA.getRows();
        size_t nA = dimA.getColumns();
        Dimensions dimB = B.getDimensions();
        size_t mB = dimB.getRows();
        size_t nB = dimB.getColumns();
        if (A.isScalar()) {
            if (!B.is2D()) {
                for (size_t k = 0; k < Clen; k++) {
                    ptrCz[k] = ptrAz[0] - ptrBz[k];
                }
            } else {
                size_t mC = dimsC.getRows();
                size_t nC = dimsC.getColumns();
                Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matC(
                    ptrCz, mC, nC);
                Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matB(
                    ptrBz, mB, nB);
                matC = ptrAz[0] - matB.array();
            }
        } else if (B.isScalar()) {
            if (!A.isScalar()) {
                for (size_t k = 0; k < Clen; k++) {
                    ptrCz[k] = ptrAz[k] - ptrBz[0];
                }
            } else {
                size_t mC = dimsC.getRows();
                size_t nC = dimsC.getColumns();
                Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matC(
                    ptrCz, mC, nC);
                Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matA(
                    ptrAz, mA, nA);
                matC = matA.array() - ptrBz[0];
            }
        } else {
            Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, 1>> matC(
                ptrCz, dimsC.getElementCount(), 1);
            Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, 1>> matA(
                ptrAz, A.getDimensions().getElementCount(), 1);
            Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, 1>> matB(
                ptrBz, B.getDimensions().getElementCount(), 1);
            matC = matA - matB;
        }
        res = ArrayOf(commonClassComplex, dimsC, ptrC, false);
    }
    if (res.allReal()) {
        res.promoteType(commonClassReal);
    }
    bSuccess = true;
    return res;
}
//=============================================================================
ArrayOf
Substraction(ArrayOf& A, ArrayOf& B, bool mustRaiseError, bool& bSuccess)
{
    bSuccess = false;
    if (A.isSparse() || B.isSparse()) {
        if (mustRaiseError) {
            std::string overload = ClassName(A) + "_minus_" + ClassName(B);
            throw Exception(_("function") + " " + overload + " " + _("undefined."));
        } else {
            return ArrayOf();
        }
    }
    if ((A.isNdArrayDoubleType(true) || A.isDoubleType(true))
        && (B.isNdArrayDoubleType(true) || B.isDoubleType(true))) {
        return real_minus_real<double>(NLS_DOUBLE, A, B, mustRaiseError, bSuccess);
    }
    if ((A.isNdArraySingleType(true) || A.isSingleType(true))
        && (B.isNdArraySingleType(true) || B.isSingleType(true))) {
        return real_minus_real<single>(NLS_SINGLE, A, B, mustRaiseError, bSuccess);
    }
    if ((A.isNdArrayDoubleType(false) || A.isDoubleType(false))
        && (B.isNdArrayDoubleType(false) || B.isDoubleType(false))) {
        if (A.getDataClass() == B.getDataClass()) {
            return complex_minus_complex<double>(
                NLS_DCOMPLEX, NLS_DOUBLE, A, B, mustRaiseError, bSuccess);
        } else {
            A.promoteType(NLS_DCOMPLEX);
            B.promoteType(NLS_DCOMPLEX);
            return complex_minus_complex<double>(
                NLS_DCOMPLEX, NLS_DOUBLE, A, B, mustRaiseError, bSuccess);
        }
    }
    if ((A.isNdArraySingleType(false) || A.isSingleType(false))
        && (B.isNdArraySingleType(false) || B.isSingleType(false))) {
        if (A.getDataClass() == B.getDataClass()) {
            return complex_minus_complex<single>(
                NLS_SCOMPLEX, NLS_SINGLE, A, B, mustRaiseError, bSuccess);
        } else {
            A.promoteType(NLS_SCOMPLEX);
            B.promoteType(NLS_SCOMPLEX);
            return complex_minus_complex<single>(
                NLS_SCOMPLEX, NLS_SINGLE, A, B, mustRaiseError, bSuccess);
        }
    }
    if (mustRaiseError) {
        std::string overload = ClassName(A) + "_minus_" + ClassName(B);
        throw Exception(_("function") + " " + overload + " " + _("undefined."));
    }
    return ArrayOf();
}
//=============================================================================
ArrayOf
empty_minus_generic(ArrayOf A, ArrayOf B, bool mustRaiseError, bool& bSuccess)
{
    ArrayOf res;
    Dimensions dimA = A.getDimensions();
    size_t mA = dimA.getRows();
    size_t nA = dimA.getColumns();
    if (mA == nA) {
        if (B.isEmpty()) {
            Dimensions dimB = B.getDimensions();
            size_t mB = dimB.getRows();
            size_t nB = dimB.getColumns();
            if ((mB == mA) && (nA == nB)) {
                bSuccess = true;
                return ArrayOf(A);
            } else {
                if (mustRaiseError) {
                    throw Exception(_W("using operator '-' \n Matrix dimensions must agree."));
                } else {
                    bSuccess = false;
                    return ArrayOf();
                }
            }
        }
        if (B.isScalar()) {
            // [] - X returns []
            bSuccess = true;
            return ArrayOf(A);
        } else {
            if (mustRaiseError) {
                throw Exception(_W("using operator '-' \n Matrix dimensions must agree."));
            } else {
                bSuccess = false;
                return ArrayOf();
            }
        }
    }
    res = ArrayOf(A);
    bSuccess = true;
    return res;
}
//=============================================================================
Dimensions
getOutputDimensions(ArrayOf A, ArrayOf B)
{
    Dimensions outputDimensions;
    if (A.isScalar()) {
        outputDimensions = B.getDimensions();
    } else {
        outputDimensions = A.getDimensions();
    }
    return outputDimensions;
}
//=============================================================================
void
checkDimensions(ArrayOf A, ArrayOf B)
{
    if (!(SameSizeCheck(A.getDimensions(), B.getDimensions()) || A.isScalar() || B.isScalar())) {
        throw Exception(_W("Size mismatch on arguments to arithmetic operator ") + L"-");
    }
}
//=============================================================================
}
//=============================================================================
