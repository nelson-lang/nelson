//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nlsBuildConfig.h"
#include "lapack_eigen_config.hpp"
#include "ExpMatrix.hpp"
#include "ClassName.hpp"
#include <unsupported/Eigen/MatrixFunctions>
#include "Error.hpp"
#include "i18n.hpp"
#include "NaN.hpp"
#include "ReciprocalConditionNumber.hpp"
#include "InverseMatrix.hpp"
#include "GeneralizedEigenDecomposition.hpp"
#include "EigenDecomposition.hpp"
#include "IsHermitian.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static void
checkIsSupportedType(const ArrayOf& A);
//=============================================================================
static void
checkIsSquare(const ArrayOf& A);
//=============================================================================
static bool
isDiagonal(const ArrayOf& A);
//=============================================================================
static bool
isAllFinite(const ArrayOf& A);
//=============================================================================
static ArrayOf
ExpDiagonalMatrix(const ArrayOf& A);
//=============================================================================
static bool
performEigenDecomposition(
    const ArrayOf& A, ArrayOf& V, ArrayOf& D, bool& needToOverload, std::wstring& errorMessage);
//=============================================================================
static ArrayOf
ExpScalarMatrix(const ArrayOf& A);
//=============================================================================
static ArrayOf
ExpSingularMatrix(const ArrayOf& A);
//=============================================================================
template <class T>
ArrayOf
ExpMatrixReal(const ArrayOf& A);
//=============================================================================
template <class T>
ArrayOf
ExpMatrixComplex(const ArrayOf& A, NelsonType destinationType);
//=============================================================================
ArrayOf
ExpMatrix(const ArrayOf& _A)
{
    checkIsSupportedType(_A);
    checkIsSquare(_A);
    if (_A.isEmpty()) {
        ArrayOf RES(_A);
        RES.ensureSingleOwner();
        return RES;
    }
    ArrayOf A(_A);
    A.ensureSingleOwner();
    if (A.isSparse()) {
        A.makeDense();
    }
    if (!isAllFinite(A)) {
        return NaN(A.getRows(), A.getColumns());
    }
    if (A.isScalar()) {
        return ExpScalarMatrix(A);
    }
    if (isDiagonal(A)) {
        return ExpDiagonalMatrix(A);
    }
    ArrayOf rcond = ReciprocalConditionNumber(A);
    bool isSingular = false;
    if (A.isDoubleType()) {
        isSingular = rcond.getContentAsDoubleScalar() < DBL_EPSILON;
    } else {
        isSingular = rcond.getContentAsSingleScalar() < FLT_EPSILON;
    }
    if (isSingular) {
        return ExpSingularMatrix(A);
    }
    switch (A.getDataClass()) {
    case NLS_DOUBLE: {
        return ExpMatrixReal<double>(A);
    } break;
    case NLS_SINGLE: {
        return ExpMatrixReal<single>(A);
    } break;
    case NLS_DCOMPLEX: {
        return ExpMatrixComplex<double>(A, NLS_DOUBLE);
    } break;
    case NLS_SCOMPLEX: {
        return ExpMatrixComplex<single>(A, NLS_SINGLE);
    } break;
    }
    return {};
}
//=============================================================================
void
checkIsSupportedType(const ArrayOf& A)
{
    bool isSupportedTypes = (A.getDataClass() == NLS_DOUBLE || A.getDataClass() == NLS_SINGLE
        || A.getDataClass() == NLS_DCOMPLEX || A.getDataClass() == NLS_SCOMPLEX);
    if (!isSupportedTypes) {
        Error(_("Undefined function 'expm' for input arguments of type") + " '" + ClassName(A)
            + "'.");
    }
}
//=============================================================================
void
checkIsSquare(const ArrayOf& A)
{
    if (!A.isSquare()) {
        Error(_("Square matrix expected."));
    }
}
//=============================================================================
template <typename OriginCType, typename destinationCType, typename EigenMatrixType>
bool
isMatrixDiagonal(const ArrayOf& A)
{
    destinationCType* ptrA = reinterpret_cast<destinationCType*>((OriginCType*)A.getDataPointer());
    Eigen::Map<EigenMatrixType> matA(ptrA, (Eigen::Index)A.getRows(), (Eigen::Index)A.getColumns());
    return matA.isDiagonal();
}
//=============================================================================
bool
isDiagonal(const ArrayOf& A)
{
    switch (A.getDataClass()) {
    case NLS_DOUBLE:
        return isMatrixDiagonal<double, double, Eigen::MatrixXd>(A);
    case NLS_DCOMPLEX:
        return isMatrixDiagonal<double, std::complex<double>, Eigen::MatrixXcd>(A);
    case NLS_SINGLE:
        return isMatrixDiagonal<single, single, Eigen::MatrixXf>(A);
    case NLS_SCOMPLEX:
        return isMatrixDiagonal<single, std::complex<single>, Eigen::MatrixXcf>(A);
    default:
        return false;
    }
}
//=============================================================================
template <typename OriginCType, typename destinationCType, typename EigenMatrixType>
bool
isMatrixAllFinite(const ArrayOf& A)
{
    destinationCType* ptrA = reinterpret_cast<destinationCType*>((OriginCType*)A.getDataPointer());
    Eigen::Map<EigenMatrixType> matA(ptrA, (Eigen::Index)A.getRows(), (Eigen::Index)A.getColumns());
    return matA.allFinite();
}
//=============================================================================
bool
isAllFinite(const ArrayOf& A)
{
    switch (A.getDataClass()) {
    case NLS_DOUBLE:
        return isMatrixAllFinite<double, double, Eigen::MatrixXd>(A);
    case NLS_DCOMPLEX:
        return isMatrixAllFinite<double, std::complex<double>, Eigen::MatrixXcd>(A);
    case NLS_SINGLE:
        return isMatrixAllFinite<single, single, Eigen::MatrixXf>(A);
    case NLS_SCOMPLEX:
        return isMatrixAllFinite<single, std::complex<single>, Eigen::MatrixXcf>(A);
    default:
        return false;
    }
    return false;
}
//=============================================================================
template <typename OriginCType, typename destinationCType, typename EigenMatrixType>
ArrayOf
ExpDiagonalMatrixImp(const ArrayOf& A)
{
    ArrayOf R(A);
    destinationCType* ptrR = reinterpret_cast<destinationCType*>((OriginCType*)R.getDataPointer());
    Eigen::Map<EigenMatrixType> matR(ptrR, (Eigen::Index)R.getRows(), (Eigen::Index)R.getColumns());
    matR.diagonal() = matR.diagonal().array().exp().matrix();
    return R;
}
//=============================================================================
ArrayOf
ExpDiagonalMatrix(const ArrayOf& A)
{
    switch (A.getDataClass()) {
    case NLS_DOUBLE:
        return ExpDiagonalMatrixImp<double, double, Eigen::MatrixXd>(A);
    case NLS_DCOMPLEX:
        return ExpDiagonalMatrixImp<double, std::complex<double>, Eigen::MatrixXcd>(A);
    case NLS_SINGLE:
        return ExpDiagonalMatrixImp<single, single, Eigen::MatrixXf>(A);
    case NLS_SCOMPLEX:
        return ExpDiagonalMatrixImp<single, std::complex<single>, Eigen::MatrixXcf>(A);
    default:
        return {};
    }
    return {};
}
//=============================================================================
ArrayOf
ExpScalarMatrix(const ArrayOf& A)
{
    switch (A.getDataClass()) {
    case NLS_DOUBLE: {
        return ArrayOf::doubleConstructor(std::exp(A.getContentAsDoubleScalar()));
    } break;
    case NLS_DCOMPLEX: {
        std::complex<double> v = std::exp(A.getContentAsDoubleComplexScalar());
        if (v.imag() == 0.) {
            return ArrayOf::doubleConstructor(v.real());
        }
        return ArrayOf::dcomplexConstructor(v.real(), v.imag());
    } break;
    case NLS_SINGLE: {
        return ArrayOf::singleConstructor(std::exp(A.getContentAsSingleScalar()));
    } break;
    case NLS_SCOMPLEX: {
        std::complex<single> v = A.getContentAsSingleComplexScalar();
        v = std::exp(v);
        if (v.imag() == 0.) {
            return ArrayOf::singleConstructor(v.real());
        }
        return ArrayOf::complexConstructor(v.real(), v.imag());
    } break;
    default:
        return {};
    }
    return {};
}
//=============================================================================
bool
performEigenDecomposition(
    const ArrayOf& A, ArrayOf& V, ArrayOf& D, bool& needToOverload, std::wstring& errorMessage)
{
    bool isHermitianA = IsHermitianWithoutSkew(A, needToOverload);
    if (isHermitianA) {
        return EigenDecompositionFullSymmetric(A, V, D, needToOverload, errorMessage);
    } else {
        return EigenDecompositionFullGeneral(A, true, V, D, needToOverload, errorMessage);
    }
}
//=============================================================================
template <typename OriginCType, typename destinationCType, typename EigenMatrixType>
void
computeMatrixExponential(const ArrayOf& V, const ArrayOf& D, const ArrayOf& invV, ArrayOf& R)
{
    destinationCType* ptrR = reinterpret_cast<destinationCType*>((OriginCType*)R.getDataPointer());
    destinationCType* ptrV = reinterpret_cast<destinationCType*>((OriginCType*)V.getDataPointer());
    destinationCType* ptrInvV
        = reinterpret_cast<destinationCType*>((OriginCType*)invV.getDataPointer());
    destinationCType* ptrD = reinterpret_cast<destinationCType*>((OriginCType*)D.getDataPointer());

    Eigen::Map<EigenMatrixType> matR(ptrR, (Eigen::Index)R.getRows(), (Eigen::Index)R.getColumns());
    Eigen::Map<EigenMatrixType> matV(ptrV, (Eigen::Index)V.getRows(), (Eigen::Index)V.getColumns());
    Eigen::Map<EigenMatrixType> matInvV(
        ptrInvV, (Eigen::Index)invV.getRows(), (Eigen::Index)invV.getColumns());
    Eigen::Map<EigenMatrixType> matD(ptrD, (Eigen::Index)D.getRows(), (Eigen::Index)D.getColumns());

#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
    for (ompIndexType i = 0; i < (ompIndexType)matD.rows(); ++i) {
        matD(i, i) = std::exp(matD(i, i));
    }

    matR.noalias() = matV * matD * matInvV;
}
//=============================================================================
ArrayOf
ExpSingularMatrix(const ArrayOf& A)
{
    // [V, D] = eig(A);
    bool needToOverload = false;
    ArrayOf V, D;
    std::wstring errorMessage;

    if (!performEigenDecomposition(A, V, D, needToOverload, errorMessage)) {
        Error(errorMessage);
    }
    ArrayOf R(A);

    ArrayOf invV = InverseMatrix(V, needToOverload);

    // expm = V * diag(exp(diag(D))) * inv(V);
    NelsonType commonType;
    if (V.isComplex() || D.isComplex() || invV.isComplex()) {
        commonType = A.isDoubleType() ? NLS_DCOMPLEX : NLS_SCOMPLEX;
    } else {
        commonType = A.isDoubleType() ? NLS_DOUBLE : NLS_SINGLE;
    }
    R.promoteType(commonType);
    V.promoteType(commonType);
    D.promoteType(commonType);
    invV.promoteType(commonType);

    switch (commonType) {
    case NLS_DOUBLE: {
        computeMatrixExponential<double, double, Eigen::MatrixXd>(V, D, invV, R);
    } break;
    case NLS_SINGLE: {
        computeMatrixExponential<single, single, Eigen::MatrixXf>(V, D, invV, R);
    } break;
    case NLS_DCOMPLEX: {
        computeMatrixExponential<double, std::complex<double>, Eigen::MatrixXcd>(V, D, invV, R);
    } break;
    case NLS_SCOMPLEX: {
        computeMatrixExponential<single, std::complex<single>, Eigen::MatrixXcf>(V, D, invV, R);
    } break;
    default: {
        Error(_W("Unsupported matrix type."));
    } break;
    }
    if (A.isDoubleType(true)) {
        R.promoteType(NLS_DOUBLE);
        return R;
    }
    if (A.isSingleType(true)) {
        R.promoteType(NLS_SINGLE);
        return R;
    }
    if (R.isReal()) {
        R.promoteType(R.isDoubleClass() ? NLS_DOUBLE : NLS_SINGLE);
    }
    return R;
}
//=============================================================================
template <class T>
ArrayOf
ExpMatrixReal(const ArrayOf& A)
{
    ArrayOf R(A);
    R.ensureSingleOwner();
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matA(
        (T*)A.getDataPointer(), (Eigen::Index)A.getRows(), (Eigen::Index)A.getColumns());
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matR(
        (T*)R.getDataPointer(), (Eigen::Index)R.getRows(), (Eigen::Index)R.getColumns());
    matR = matA.exp();
    return R;
}
//=============================================================================
template <class T>
ArrayOf
ExpMatrixComplex(const ArrayOf& A, NelsonType destinationType)
{
    ArrayOf R(A);
    R.ensureSingleOwner();
    auto* Az = reinterpret_cast<std::complex<T>*>((T*)A.getDataPointer());
    auto* Rz = reinterpret_cast<std::complex<T>*>((T*)R.getDataPointer());
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matA(
        Az, (Eigen::Index)A.getRows(), (Eigen::Index)A.getColumns());
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matR(
        Rz, (Eigen::Index)R.getRows(), (Eigen::Index)R.getColumns());
    // [V, D] = eig(A);
    // expm = V * diag(exp(diag(D))) * inv(V);
    Eigen::ComplexEigenSolver<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> es(
        matA);
    auto evects = es.eigenvectors();
    auto evals = es.eigenvalues();
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
    for (ompIndexType i = 0; i < static_cast<ompIndexType>(evals.rows()); ++i) {
        evals(i) = std::exp(evals(i));
    }
    auto evalsdiag = evals.asDiagonal();
    matR = evects * evalsdiag * evects.inverse();
    if (A.getDataClass() == destinationType) {
        R.promoteType(destinationType);
        return R;
    }
    if (R.allReal()) {
        R.promoteType(destinationType);
    }
    return R;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
