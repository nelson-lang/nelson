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
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "Decomplexify.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static void
complexTransposeInPlace(ArrayOf& A, bool& needToOverload);
//=============================================================================
static ArrayOf
zeroRightDivideResult(NelsonType dataClass, const Dimensions& dimsC)
{
    if (dimsC.elementCount() == 0) {
        ArrayOf res = ArrayOf::empty(dimsC);
        res.promoteType(dataClass);
        return res;
    }
    void* ptrC = ArrayOf::allocateArrayOf(dataClass, dimsC.elementCount(), stringVector(), true);
    return ArrayOf(dataClass, dimsC, ptrC, false);
}
//=============================================================================
ArrayOf
RightDivide(const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    // a quotient with an empty operand is empty or all zeros: it is real
    if (A.isEmpty() || B.isEmpty()) {
        Dimensions dimsA = A.dimensions();
        Dimensions dimsB = B.dimensions();

        if (A.isEmpty() && B.isScalar()) {
            return decomplexify(A);
        }
        if (B.isEmpty() && A.isScalar()) {
            if (dimsB.columns() == 1 && dimsB.is2D()) {
                return ArrayOf::empty(Dimensions(1, 0));
            }
            Error("Nelson:dimagree:sizeMismatchArgumentsArithmeticOperatorValue",
                "Size mismatch on arguments to arithmetic operator %s", "/");
        }
        dimsA.simplify();
        dimsB.simplify();
        if (dimsA.columns() != dimsB.columns()) {
            Error("Nelson:dimagree:sizeMismatchArgumentsArithmeticOperatorValue",
                "Size mismatch on arguments to arithmetic operator %s", "/");
        }
        if (dimsA.equals(dimsB)) {
            return decomplexify(A);
        }
        if (dimsA.length() > 2 || dimsB.length() > 2) {
            Error(L"Nelson:expected2D", L"%s must be 2-D.", L"Input arguments");
        }
        NelsonType commonClass = A.dataClass();
        Dimensions dimsC(dimsA[0], dimsB[0]);
        return decomplexify(zeroRightDivideResult(commonClass, dimsC));
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
    if (R.isSparse()) {
        return ComplexTranspose(R, needToOverload);
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
        (T*)ptrA, dimsA.rows(), dimsA.columns());
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matTransposed(
        (T*)ptrRes, dimsA.columns(), dimsA.rows());
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
        matCplxA, dimsA.rows(), dimsA.columns());
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matTransposed(
        matCplxRes, dimsA.columns(), dimsA.rows());
    matTransposed = matOrigin.conjugate().transpose().eval();
}
//=============================================================================
void
complexTransposeInPlace(ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    // Raw in-place payload writes below: detach in case A is ever shared.
    A.ensureSingleOwner();
    Dimensions dimsA = A.dimensions();
    Dimensions dimsTranspose(dimsA.columns(), dimsA.rows());
    switch (A.dataClass()) {
    case NLS_DOUBLE: {
        auto* ptrA = (double*)A.dataPointer();
        complexTransposeInPlaceRealTemplate<double>(dimsA, (double*)ptrA, (double*)ptrA);
    } break;
    case NLS_SINGLE: {
        auto* ptrA = (single*)A.dataPointer();
        complexTransposeInPlaceRealTemplate<single>(dimsA, (single*)ptrA, (single*)ptrA);
    } break;
    case NLS_DCOMPLEX: {
        auto* ptrA = (double*)A.dataPointer();
        complexTransposeInPlaceComplexTemplate<double>(dimsA, (double*)ptrA, (double*)ptrA);
    } break;
    case NLS_SCOMPLEX: {
        auto* ptrA = (single*)A.dataPointer();
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
