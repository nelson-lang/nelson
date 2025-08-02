//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "lapack_eigen_config.hpp"
#include <Eigen/Dense>
#include "RightDivide.hpp"
#include "DotRightDivide.hpp"
#include "LeftDivide.hpp"
#include "MatrixCheck.hpp"
#include "ComplexTranspose.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static void
complexTransposeInPlace(ArrayOf& A, bool& needToOverload);
//=============================================================================
ArrayOf
RightDivide(const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
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
            }
            Error(_("Size mismatch on arguments to arithmetic operator") + " " + "/");
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
            NelsonType commonClass = A.getDataClass();
            Dimensions dimsC(dimsA[0], dimsB[0]);
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
    ArrayOf _A = ComplexTranspose(A, needToOverload);
    if (needToOverload) {
        return R;
    }
    ArrayOf _B = ComplexTranspose(B, needToOverload);
    if (needToOverload) {
        return R;
    }
    R = LeftDivide(_B, _A, needToOverload);
    if (needToOverload) {
        return R;
    }
    complexTransposeInPlace(R, needToOverload);
    return R;
}
//=============================================================================
template <class T>
void
complexTransposeInPlaceRealTemplate(const Dimensions& dimsA, T* ptrA, T* ptrRes)
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
complexTransposeInPlaceComplexTemplate(const Dimensions& dimsA, T* ptrA, T* ptrRes)
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
