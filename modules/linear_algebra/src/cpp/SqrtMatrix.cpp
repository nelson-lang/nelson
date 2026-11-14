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
#define _SCL_SECURE_NO_WARNINGS
// The singular-root branch divides by an exact zero on purpose to produce the expected Inf.
#pragma warning(disable : 4723)
#endif
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "lapack_eigen_config.hpp"
#include <unsupported/Eigen/MatrixFunctions>
#include "SqrtMatrix.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "SchurDecomposition.hpp"
#include "Warning.hpp"
#include "i18n.hpp"
#include <cmath>
//=============================================================================
namespace Nelson {
//=============================================================================
// A complex product where an exactly-zero operand annihilates to zero, even against an
// infinity (plain 0 * Inf is NaN), and a real operand (zero imaginary part) multiplies
// through its real part only so it never forms a 0 * Inf cross term. A nilpotent or singular
// matrix's triangular root holds infinities; the unitary Schur back-transform must not let
// them spread NaN into entries the unitary factor zeroes out.
template <class T>
static inline std::complex<T>
robustComplexProduct(const std::complex<T>& a, const std::complex<T>& b)
{
    if ((a.real() == (T)0 && a.imag() == (T)0) || (b.real() == (T)0 && b.imag() == (T)0)) {
        return std::complex<T>((T)0, (T)0);
    }
    if (a.imag() == (T)0) {
        return std::complex<T>(a.real() * b.real(), a.real() * b.imag());
    }
    if (b.imag() == (T)0) {
        return std::complex<T>(a.real() * b.real(), a.imag() * b.real());
    }
    return a * b;
}
//=============================================================================
// Matrix product built on robustComplexProduct, used only for the back-transform of a
// non-finite triangular root so the normal finite path keeps Eigen's fast product.
template <class T>
static Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>
robustMatrixProduct(const Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>& A,
    const Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>& B)
{
    using MatrixType = Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>;
    MatrixType C = MatrixType::Zero(A.rows(), B.cols());
    for (Eigen::Index j = 0; j < B.cols(); ++j) {
        for (Eigen::Index i = 0; i < A.rows(); ++i) {
            std::complex<T> sum((T)0, (T)0);
            for (Eigen::Index k = 0; k < A.cols(); ++k) {
                sum += robustComplexProduct(A(i, k), B(k, j));
            }
            C(i, j) = sum;
        }
    }
    return C;
}
//=============================================================================
template <class T>
ArrayOf
sqrtmComplex(ArrayOf& A)
{
    const auto* inputData = reinterpret_cast<const std::complex<T>*>(A.dataPointer());
    for (indexType k = 0; k < A.elementCount(); ++k) {
        if (!std::isfinite(static_cast<double>(inputData[k].real()))
            || !std::isfinite(static_cast<double>(inputData[k].imag()))) {
            Error("Nelson:linear_algebra:inputNotFinite", "Input must be finite.");
        }
    }
    ArrayOf U;
    ArrayOf Tschur;
    SchurDecomposition(A, true, U, Tschur);
    NelsonType complexClass = sizeof(T) == sizeof(single) ? NLS_SCOMPLEX : NLS_DCOMPLEX;
    U.promoteType(complexClass);
    Tschur.promoteType(complexClass);
    ArrayOf R(U);
    R.ensureSingleOwner();
    using MatrixType = Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>;
    const auto* up = reinterpret_cast<const std::complex<T>*>(U.dataPointer());
    const auto* tp = reinterpret_cast<const std::complex<T>*>(Tschur.dataPointer());
    auto* rp = reinterpret_cast<std::complex<T>*>(const_cast<void*>(R.dataPointer()));
    Eigen::Map<const MatrixType> matU(up, (Eigen::Index)U.rows(), (Eigen::Index)U.columns());
    Eigen::Map<const MatrixType> matT(
        tp, (Eigen::Index)Tschur.rows(), (Eigen::Index)Tschur.columns());
    Eigen::Map<MatrixType> matR(rp, (Eigen::Index)R.rows(), (Eigen::Index)R.columns());
    bool isSingular = false;
    for (Eigen::Index i = 0; i < matT.rows(); ++i) {
        if (matT(i, i) == std::complex<T>((T)0, (T)0)) {
            isSingular = true;
            break;
        }
    }
    // column by column root of the triangular factor; a zero pivot sum over a zero numerator
    // gives 0 (a singular matrix such as zeros keeps a finite root), otherwise an infinity
    Eigen::Index order = matT.rows();
    MatrixType sqrtT = MatrixType::Zero(order, order);
    for (Eigen::Index j = 0; j < order; ++j) {
        sqrtT(j, j) = std::sqrt(matT(j, j));
        for (Eigen::Index i = j - 1; i >= 0; --i) {
            Eigen::Index span = j - i - 1;
            std::complex<T> sum = span > 0
                ? std::complex<T>(sqrtT.row(i)
                          .segment(i + 1, span)
                          .transpose()
                          .cwiseProduct(sqrtT.col(j).segment(i + 1, span))
                          .sum())
                : std::complex<T>((T)0, (T)0);
            std::complex<T> numerator = matT(i, j) - sum;
            std::complex<T> denominator = sqrtT(i, i) + sqrtT(j, j);
            if (denominator == std::complex<T>((T)0, (T)0)) {
                sqrtT(i, j) = numerator == std::complex<T>((T)0, (T)0)
                    ? std::complex<T>((T)0, (T)0)
                    : std::complex<T>(numerator.real() / (T)0,
                          numerator.imag() == (T)0 ? (T)0 : numerator.imag() / (T)0);
            } else {
                sqrtT(i, j) = numerator / denominator;
            }
        }
    }
    bool sqrtHasNonFinite = false;
    for (Eigen::Index j = 0; j < order && !sqrtHasNonFinite; ++j) {
        for (Eigen::Index i = 0; i < order; ++i) {
            if (!std::isfinite(static_cast<double>(sqrtT(i, j).real()))
                || !std::isfinite(static_cast<double>(sqrtT(i, j).imag()))) {
                sqrtHasNonFinite = true;
                break;
            }
        }
    }
    if (sqrtHasNonFinite) {
        // Keep the infinities of a nilpotent/singular root from turning into NaN where the
        // unitary factor has zeros: multiply with the zero-annihilating product instead.
        MatrixType matUdense = matU;
        MatrixType matUadj = matU.adjoint();
        matR = robustMatrixProduct<T>(robustMatrixProduct<T>(matUdense, sqrtT), matUadj);
    } else {
        matR = matU * sqrtT * matU.adjoint();
    }
    if (isSingular) {
        Warning(
            "Nelson:sqrtm:SingularMatrix", "Matrix is singular and may not have a square root.");
    }
    return R;
}
//=============================================================================
ArrayOf
SqrtMatrix(ArrayOf A)
{
    bool isSupportedTypes = (A.dataClass() == NLS_DOUBLE || A.dataClass() == NLS_SINGLE
                                || A.dataClass() == NLS_DCOMPLEX || A.dataClass() == NLS_SCOMPLEX)
        && !A.isSparse();
    if (!isSupportedTypes) {
        Error("Nelson:UndefinedFunction:undefinedFunctionValueInputArgumentsType",
            "Undefined function '%s' for input arguments of type '%s'.", "sqrtm", ClassName(A));
    }
    if (!A.isSquare()) {
        Error("Nelson:linear_algebra:squareMatrixExpected", "%s expected.", "Square matrix");
    }
    if (A.isEmpty()) {
        ArrayOf res(A);
        res.ensureSingleOwner();
        return res;
    }
    ArrayOf res;
    if (A.dataClass() == NLS_DOUBLE || A.dataClass() == NLS_DCOMPLEX) {
        if (A.dataClass() == NLS_DOUBLE) {
            A.promoteType(NLS_DCOMPLEX);
        }
        res = sqrtmComplex<double>(A);
        if (res.allReal()) {
            res.promoteType(NLS_DOUBLE);
        }
    } else {
        if (A.dataClass() == NLS_SINGLE) {
            A.promoteType(NLS_SCOMPLEX);
        }
        res = sqrtmComplex<single>(A);
        if (res.allReal()) {
            res.promoteType(NLS_SINGLE);
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
