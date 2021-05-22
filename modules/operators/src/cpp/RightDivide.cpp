//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <Eigen/Dense>
#include "RightDivide.hpp"
#include "DotRightDivide.hpp"
#include "LeftDivide.hpp"
#include "MatrixCheck.hpp"
#include "ComplexTranspose.hpp"
#include "FindCommonClass.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static void
complexTransposeInPlace(ArrayOf& A, bool& needToOverload);
//=============================================================================
ArrayOf
RightDivide(ArrayOf A, ArrayOf B, bool& needToOverload)
{
    if (A.isEmpty() || B.isEmpty()) {
        Dimensions dimsA = A.getDimensions();
        Dimensions dimsB = B.getDimensions();

        if (A.isEmpty() && B.isScalar()) {
            return A;
        }
        if (B.isEmpty() && A.isScalar()) {
            if (dimsB.getColumns() == 1 && dimsB.is2D()) {
                return ArrayOf::emptyConstructor(Dimensions(1, 0));
            } else {
                Error(_("Size mismatch on arguments to arithmetic operator") + " " + "/");
            }
        }
        dimsA.simplify();
        dimsB.simplify();
        if (dimsA.getColumns() != dimsB.getColumns()) {
            Error(_("Size mismatch on arguments to arithmetic operator") + " " + "/");
        }
        if (dimsA.equals(dimsB)) {
            return A;
        }
        if (dimsA.getLength() > 2 || dimsB.getLength() > 2) {
            Error(ERROR_WRONG_ARGUMENTS_SIZE_2D_MATRIX_EXPECTED);
        }
        if (dimsA[0] != 0 && dimsB[0] != 0) {
            Dimensions dimsC(dimsA[0], dimsB[0]);
            Class commonClass = FindCommonClass(A, B, needToOverload);
            void* pT = ArrayOf::allocateArrayOf(
                commonClass, dimsC.getElementCount(), stringVector(), true);
            return ArrayOf(commonClass, dimsC, pT, false);
        }
        Error(_("Size mismatch on arguments to arithmetic operator") + " " + "/");
    }

    if (B.isScalar()) {
        return DotRightDivide(A, B, needToOverload);
    }
    ArrayOf R;
    A = ComplexTranspose(A, needToOverload);
    if (needToOverload) {
        return R;
    }
    B = ComplexTranspose(B, needToOverload);
    if (needToOverload) {
        return R;
    }
    R = LeftDivide(B, A, needToOverload);
    if (needToOverload) {
        return R;
    }
    complexTransposeInPlace(R, needToOverload);
    return R;
}
//=============================================================================
template <class T>
void
complexTransposeInPlaceRealTemplate(const Dimensions &dimsA, T* ptrA, T* ptrRes)
{
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matOrigin(
        (T*)ptrA, dimsA.getRows(), dimsA.getColumns());
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matTransposed(
        (T*)ptrRes, dimsA.getColumns(), dimsA.getRows());
    matTransposed = matOrigin.conjugate().transpose().eval();
}
//=============================================================================
template <class T>
void
complexTransposeInPlaceComplexTemplate(const Dimensions &dimsA, T* ptrA, T* ptrRes)
{
    std::complex<T>* matCplxA = reinterpret_cast<std::complex<T>*>(ptrA);
    std::complex<T>* matCplxRes = reinterpret_cast<std::complex<T>*>(ptrRes);
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matOrigin(
        matCplxA, dimsA.getRows(), dimsA.getColumns());
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matTransposed(
        matCplxRes, dimsA.getColumns(), dimsA.getRows());
    matTransposed = matOrigin.conjugate().transpose().eval();
}
//=============================================================================
void
complexTransposeInPlace(ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    Dimensions dimsA = A.getDimensions();
    Dimensions dimsTranspose(dimsA.getColumns(), dimsA.getRows());
    switch (A.getDataClass()) {
    case NLS_DOUBLE: {
        auto* ptrA = (double*)A.getDataPointer();
        complexTransposeInPlaceRealTemplate<double>(dimsA, (double*)ptrA, (double*)ptrA);
    } break;
    case NLS_SINGLE: {
        auto* ptrA = (single*)A.getDataPointer();
        complexTransposeInPlaceRealTemplate<single>(dimsA, (single*)ptrA, (single*)ptrA);
    } break;
    case NLS_DCOMPLEX: {
        auto* ptrA = (double*)A.getDataPointer();
        complexTransposeInPlaceComplexTemplate<double>(dimsA, (double*)ptrA, (double*)ptrA);
    } break;
    case NLS_SCOMPLEX: {
        auto* ptrA = (single*)A.getDataPointer();
        complexTransposeInPlaceComplexTemplate<single>(dimsA, (single*)ptrA, (single*)ptrA);
    } break;
    default: {
        needToOverload = true;
    } break;
    }
    if (!needToOverload) {
        A.changeInPlaceDimensions(dimsTranspose);
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
