//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "lapack_eigen_config.hpp"
#include "ExpMatrix.hpp"
#include "ClassName.hpp"
#include <unsupported/Eigen/MatrixFunctions>
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "NaN.hpp"
#include "ReciprocalConditionNumber.hpp"
#include "InverseMatrix.hpp"
#include "GeneralizedEigenDecomposition.hpp"
#include "EigenDecomposition.hpp"
#include "IsHermitian.hpp"
#include <algorithm>
#include <cmath>
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
static double
maxAbsoluteValue(const ArrayOf& A);
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
static bool
tryExpMatrixSafelyDiagonalizable(const ArrayOf& A, ArrayOf& R);
//=============================================================================
static ArrayOf
ExpMatrixFromEigenDecomposition(const ArrayOf& A, ArrayOf& V, ArrayOf& D);
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
    ArrayOf A(_A);
    A.ensureSingleOwner();
    if (A.isSparse()) {
        A.makeDense();
    }
    if (A.isEmpty()) {
        return A;
    }
    if (A.isComplex() && A.allReal()) {
        A.promoteType(A.isDoubleClass() ? NLS_DOUBLE : NLS_SINGLE);
    }
    if (!isAllFinite(A)) {
        return NaN(A.rows(), A.columns());
    }
    if (A.isScalar()) {
        return ExpScalarMatrix(A);
    }
    if (isDiagonal(A)) {
        return ExpDiagonalMatrix(A);
    }
    // The scaling-and-squaring Pade method below is valid for every square
    // matrix, including singular and defective ones. An eigenvector-based
    // path (V*diag(exp(D))*inv(V)) cannot handle defective matrices such as
    // [0 1; 0 0], whose eigenvector matrix is singular.
    switch (A.dataClass()) {
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
    default:
        break;
    }
    return {};
}
//=============================================================================
void
checkIsSupportedType(const ArrayOf& A)
{
    bool isSupportedTypes = (A.dataClass() == NLS_DOUBLE || A.dataClass() == NLS_SINGLE
        || A.dataClass() == NLS_DCOMPLEX || A.dataClass() == NLS_SCOMPLEX);
    if (!isSupportedTypes) {
        Error("Nelson:UndefinedFunction:undefinedFunctionValueInputArgumentsType",
            "Undefined function '%s' for input arguments of type '%s'.", "expm", ClassName(A));
    }
}
//=============================================================================
void
checkIsSquare(const ArrayOf& A)
{
    if (!A.isSquare()) {
        Error("Nelson:linear_algebra:squareMatrixExpected", "%s expected.", "Square matrix");
    }
}
//=============================================================================
template <typename OriginCType, typename destinationCType, typename EigenMatrixType>
bool
isMatrixDiagonal(const ArrayOf& A)
{
    destinationCType* ptrA = reinterpret_cast<destinationCType*>((OriginCType*)A.dataPointer());
    Eigen::Map<EigenMatrixType> matA(ptrA, (Eigen::Index)A.rows(), (Eigen::Index)A.columns());
    return matA.isDiagonal();
}
//=============================================================================
bool
isDiagonal(const ArrayOf& A)
{
    switch (A.dataClass()) {
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
    destinationCType* ptrA = reinterpret_cast<destinationCType*>((OriginCType*)A.dataPointer());
    Eigen::Map<EigenMatrixType> matA(ptrA, (Eigen::Index)A.rows(), (Eigen::Index)A.columns());
    return matA.allFinite();
}
//=============================================================================
bool
isAllFinite(const ArrayOf& A)
{
    switch (A.dataClass()) {
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
template <typename T>
double
maxAbsoluteValueReal(const ArrayOf& A)
{
    const T* ptrA = static_cast<const T*>(A.dataPointer());
    indexType elementCount = A.elementCount();
    double maxValue = 0.;
    for (size_t k = 0; k < elementCount; ++k) {
        maxValue = std::max(maxValue, std::abs(static_cast<double>(ptrA[k])));
    }
    return maxValue;
}
//=============================================================================
double
maxAbsoluteValue(const ArrayOf& A)
{
    switch (A.dataClass()) {
    case NLS_DOUBLE:
        return maxAbsoluteValueReal<double>(A);
    case NLS_SINGLE:
        return maxAbsoluteValueReal<single>(A);
    default:
        return 0.;
    }
}
//=============================================================================
template <typename OriginCType, typename destinationCType>
ArrayOf
ExpDiagonalMatrixImp(const ArrayOf& A)
{
    ArrayOf R(A);
    destinationCType* ptrR = reinterpret_cast<destinationCType*>((OriginCType*)R.dataPointer());
    indexType rows = R.rows();
    indexType columns = R.columns();
    indexType stride = rows + 1;
    indexType diagonalLength = std::min(rows, columns);
    for (indexType k = 0; k < diagonalLength; ++k) {
        indexType offset = k * stride;
        ptrR[offset] = std::exp(ptrR[offset]);
    }
    return R;
}
//=============================================================================
ArrayOf
ExpDiagonalMatrix(const ArrayOf& A)
{
    ArrayOf R;
    switch (A.dataClass()) {
    case NLS_DOUBLE:
        R = ExpDiagonalMatrixImp<double, double>(A);
        break;
    case NLS_DCOMPLEX:
        R = ExpDiagonalMatrixImp<double, std::complex<double>>(A);
        break;
    case NLS_SINGLE:
        R = ExpDiagonalMatrixImp<single, single>(A);
        break;
    case NLS_SCOMPLEX:
        R = ExpDiagonalMatrixImp<single, std::complex<single>>(A);
        break;
    default:
        return {};
    }
    if (R.allReal()) {
        R.promoteType(R.isDoubleClass() ? NLS_DOUBLE : NLS_SINGLE);
    }
    return R;
}
//=============================================================================
ArrayOf
ExpScalarMatrix(const ArrayOf& A)
{
    switch (A.dataClass()) {
    case NLS_DOUBLE: {
        return ArrayOf::doubleScalar(std::exp(A.asDoubleScalar()));
    } break;
    case NLS_DCOMPLEX: {
        std::complex<double> scalar = A.asDoubleComplexScalar();
        if (scalar.imag() == 0.) {
            return ArrayOf::doubleScalar(std::exp(scalar.real()));
        }
        std::complex<double> v = std::exp(scalar);
        if (v.imag() == 0.) {
            return ArrayOf::doubleScalar(v.real());
        }
        return ArrayOf::dsingleComplexScalar(v.real(), v.imag());
    } break;
    case NLS_SINGLE: {
        return ArrayOf::singleScalar(std::exp(A.asSingleScalar()));
    } break;
    case NLS_SCOMPLEX: {
        std::complex<single> scalar = A.asSingleComplexScalar();
        if (scalar.imag() == 0.) {
            return ArrayOf::singleScalar(std::exp(scalar.real()));
        }
        std::complex<single> v = std::exp(scalar);
        return ArrayOf::singleComplexScalar(v.real(), v.imag());
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
bool
tryExpMatrixSafelyDiagonalizable(const ArrayOf& A, ArrayOf& R)
{
    bool needToOverload = false;
    ArrayOf V, D;
    std::wstring errorMessage;
    if (!performEigenDecomposition(A, V, D, needToOverload, errorMessage)) {
        return false;
    }
    double rcond = ReciprocalConditionNumber(V).asDoubleScalar();
    double threshold = A.isDoubleClass() ? 1e-12 : 1e-5;
    if (!std::isfinite(rcond) || rcond <= threshold) {
        return false;
    }
    R = ExpMatrixFromEigenDecomposition(A, V, D);
    return true;
}
//=============================================================================
template <typename OriginCType, typename destinationCType, typename EigenMatrixType>
void
computeMatrixExponential(const ArrayOf& V, const ArrayOf& D, const ArrayOf& invV, ArrayOf& R)
{
    destinationCType* ptrR = reinterpret_cast<destinationCType*>((OriginCType*)R.dataPointer());
    destinationCType* ptrV = reinterpret_cast<destinationCType*>((OriginCType*)V.dataPointer());
    destinationCType* ptrInvV
        = reinterpret_cast<destinationCType*>((OriginCType*)invV.dataPointer());
    destinationCType* ptrD = reinterpret_cast<destinationCType*>((OriginCType*)D.dataPointer());

    Eigen::Map<EigenMatrixType> matR(ptrR, (Eigen::Index)R.rows(), (Eigen::Index)R.columns());
    Eigen::Map<EigenMatrixType> matV(ptrV, (Eigen::Index)V.rows(), (Eigen::Index)V.columns());
    Eigen::Map<EigenMatrixType> matInvV(
        ptrInvV, (Eigen::Index)invV.rows(), (Eigen::Index)invV.columns());
    Eigen::Map<EigenMatrixType> matD(ptrD, (Eigen::Index)D.rows(), (Eigen::Index)D.columns());

    OMP_PARALLEL_FOR_LOOP(matD.rows())
    for (ompIndexType i = 0; i < (ompIndexType)matD.rows(); ++i) {
        matD(i, i) = std::exp(matD(i, i));
    }

    matR.noalias() = matV * matD * matInvV;
}
//=============================================================================
ArrayOf
ExpMatrixFromEigenDecomposition(const ArrayOf& A, ArrayOf& V, ArrayOf& D)
{
    bool needToOverload = false;
    ArrayOf R(A);

    ArrayOf invV = InverseMatrix(V, needToOverload);

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
        Error(L"Nelson:linear_algebra:unsupportedValue", L"Unsupported %s.", L"matrix type");
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
    constexpr double EIGEN_FALLBACK_MAGNITUDE = 1e8;
    if (maxAbsoluteValue(A) > EIGEN_FALLBACK_MAGNITUDE) {
        ArrayOf R;
        if (tryExpMatrixSafelyDiagonalizable(A, R)) {
            return R;
        }
    }
    ArrayOf R(A);
    R.ensureSingleOwner();
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matA(
        (T*)A.dataPointer(), (Eigen::Index)A.rows(), (Eigen::Index)A.columns());
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matR(
        (T*)R.dataPointer(), (Eigen::Index)R.rows(), (Eigen::Index)R.columns());
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
    auto* Az = reinterpret_cast<std::complex<T>*>((T*)A.dataPointer());
    auto* Rz = reinterpret_cast<std::complex<T>*>((T*)R.dataPointer());
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matA(
        Az, (Eigen::Index)A.rows(), (Eigen::Index)A.columns());
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matR(
        Rz, (Eigen::Index)R.rows(), (Eigen::Index)R.columns());
    matR = matA.exp();
    if (A.dataClass() == destinationType) {
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
