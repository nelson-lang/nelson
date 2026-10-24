//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "MatrixPower.hpp"
#include <cmath>
#include <Eigen/Eigenvalues>
#include <Eigen/Sparse>
#include <Eigen/SparseLU>
#include <type_traits>
#include <vector>
#include "DotPower.hpp"
#include "InverseMatrix.hpp"
#include "IsSymmetric.hpp"
#include "EigenDecomposition.hpp"
#include "MatrixMultiplication.hpp"
#include "ReciprocalConditionNumber.hpp"
#include "RightDivide.hpp"
#include "Eye.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "Warning.hpp"
#include "i18n.hpp"
#include "SparseArrayOf.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T> using NelsonSparseMatrix = Eigen::SparseMatrix<T, 0, signedIndexType>;
//=============================================================================
static ArrayOf
matrixScalarPower(const ArrayOf& A, const ArrayOf& B, bool& needToOverload);
//=============================================================================
// The non-integer matrix power is computed by diagonalization
// (V * diag(d .^ p) / V). When the matrix is defective (non-diagonalizable),
// V is (nearly) singular and the result may be silently wrong: emit a clear
// warning in that case instead of staying silent.
static void
warnIfIllConditionedEigenvectors(const ArrayOf& V)
{
    if (V.isEmpty() || !V.is2D()) {
        return;
    }
    ArrayOf rcondArray = ReciprocalConditionNumber(V);
    double rcondValue = rcondArray.asDoubleScalar();
    if (rcondValue < 1e-8) {
        RaiseWarning("Nelson:mpower:IllConditionedEigenvectors",
            Nelson::resolveMessageTemplate("Nelson:linear_algebra:matrixDefectiveEigenvectorsBadly",
                "Matrix is defective or eigenvectors are badly conditioned: results may be "
                "inaccurate."));
    }
}
//=============================================================================
template <class T>
static const NelsonSparseMatrix<T>*
getSparseMatrixPointer(const ArrayOf& A)
{
    return static_cast<const NelsonSparseMatrix<T>*>(A.sparseDataPointer());
}
//=============================================================================
template <class T>
static T
getSparseScalarValue(const ArrayOf& A)
{
    return getSparseMatrixPointer<T>(A)->coeff(0, 0);
}
//=============================================================================
static double
getScalarPowerValue(const ArrayOf& B)
{
    if (B.isSparse()) {
        switch (B.dataClass()) {
        case NLS_SINGLE:
            return getSparseScalarValue<single>(B);
        case NLS_DOUBLE:
            return getSparseScalarValue<double>(B);
        case NLS_SCOMPLEX: {
            singlecomplex value = getSparseScalarValue<singlecomplex>(B);
            if (value.imag() != 0) {
                Error(L"Nelson:mpower:invalidExponent",
                    L"Power (^) operator only supports real sparse scalar exponent.");
            }
            return value.real();
        } break;
        case NLS_DCOMPLEX: {
            doublecomplex value = getSparseScalarValue<doublecomplex>(B);
            if (value.imag() != 0) {
                Error(L"Nelson:mpower:invalidExponent",
                    L"Power (^) operator only supports real sparse scalar exponent.");
            }
            return value.real();
        } break;
        default:
            Error(L"Nelson:mpower:typeNotSupported", L"Type not supported.");
        }
    }
    if (B.isComplex()) {
        ArrayOf BB = B;
        BB.promoteType(NLS_DCOMPLEX);
        auto* value = static_cast<const doublecomplex*>(BB.dataPointer());
        if (value[0].imag() != 0) {
            Error(L"Nelson:mpower:invalidExponent",
                L"Power (^) operator only supports real sparse scalar exponent.");
        }
        return value[0].real();
    }
    ArrayOf BB = B;
    return BB.asDoubleScalar();
}
//=============================================================================
static bool
isRealScalarPowerExponent(const ArrayOf& B)
{
    if (!B.isScalar()) {
        return false;
    }
    if (B.isReal()) {
        return true;
    }
    return B.isComplex() && B.allReal();
}
//=============================================================================
template <class T>
static ArrayOf
sparseMatrixPowerResult(
    NelsonType dataClass, const Dimensions& dimensions, NelsonSparseMatrix<T>* sparseMatrix)
{
    constexpr bool pruneZeros
        = !std::is_same_v<T, singlecomplex> && !std::is_same_v<T, doublecomplex>;
    return makeSparseArrayOfFromEigen<T>(dataClass, dimensions, sparseMatrix, pruneZeros);
}
//=============================================================================
template <class T>
static Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>
sparseToDenseEigenMatrix(const ArrayOf& A)
{
    Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> dense(A.rows(), A.columns());
    dense.setZero();
    const auto* sparseA = getSparseMatrixPointer<T>(A);
    for (signedIndexType col = 0; col < sparseA->outerSize(); ++col) {
        for (typename NelsonSparseMatrix<T>::InnerIterator it(*sparseA, col); it; ++it) {
            dense(it.row(), it.col()) = it.value();
        }
    }
    return dense;
}
//=============================================================================
static ArrayOf
denseComplexMatrixToSparseArrayOf(NelsonType realClass, NelsonType complexClass,
    const Dimensions& dimensions, const Eigen::MatrixXcd& denseMatrix)
{
    bool allReal = true;
    for (indexType col = 0; col < dimensions.columns(); ++col) {
        for (indexType row = 0; row < dimensions.rows(); ++row) {
            doublecomplex value
                = denseMatrix(static_cast<signedIndexType>(row), static_cast<signedIndexType>(col));
            if (std::abs(value.imag()) >= 1e-12) {
                allReal = false;
                break;
            }
        }
        if (!allReal) {
            break;
        }
    }
    if (allReal) {
        if (realClass == NLS_SINGLE) {
            auto* sparseResult
                = new NelsonSparseMatrix<single>(dimensions.rows(), dimensions.columns());
            std::vector<Eigen::Triplet<single, signedIndexType>> triplets;
            triplets.reserve(static_cast<size_t>(dimensions.elementCount()));
            for (indexType col = 0; col < dimensions.columns(); ++col) {
                for (indexType row = 0; row < dimensions.rows(); ++row) {
                    double value = denseMatrix(
                        static_cast<signedIndexType>(row), static_cast<signedIndexType>(col))
                                       .real();
                    if (value != 0.) {
                        triplets.emplace_back(static_cast<signedIndexType>(row),
                            static_cast<signedIndexType>(col), static_cast<single>(value));
                    }
                }
            }
            sparseResult->setFromTriplets(triplets.begin(), triplets.end());
            return sparseMatrixPowerResult<single>(NLS_SINGLE, dimensions, sparseResult);
        }
        auto* sparseResult
            = new NelsonSparseMatrix<double>(dimensions.rows(), dimensions.columns());
        std::vector<Eigen::Triplet<double, signedIndexType>> triplets;
        triplets.reserve(static_cast<size_t>(dimensions.elementCount()));
        for (indexType col = 0; col < dimensions.columns(); ++col) {
            for (indexType row = 0; row < dimensions.rows(); ++row) {
                double value = denseMatrix(
                    static_cast<signedIndexType>(row), static_cast<signedIndexType>(col))
                                   .real();
                if (value != 0.) {
                    triplets.emplace_back(static_cast<signedIndexType>(row),
                        static_cast<signedIndexType>(col), value);
                }
            }
        }
        sparseResult->setFromTriplets(triplets.begin(), triplets.end());
        return sparseMatrixPowerResult<double>(NLS_DOUBLE, dimensions, sparseResult);
    }
    if (complexClass == NLS_SCOMPLEX) {
        auto* sparseResult
            = new NelsonSparseMatrix<singlecomplex>(dimensions.rows(), dimensions.columns());
        std::vector<Eigen::Triplet<singlecomplex, signedIndexType>> triplets;
        triplets.reserve(static_cast<size_t>(dimensions.elementCount()));
        for (indexType col = 0; col < dimensions.columns(); ++col) {
            for (indexType row = 0; row < dimensions.rows(); ++row) {
                doublecomplex value = denseMatrix(
                    static_cast<signedIndexType>(row), static_cast<signedIndexType>(col));
                if (value != doublecomplex(0, 0)) {
                    triplets.emplace_back(static_cast<signedIndexType>(row),
                        static_cast<signedIndexType>(col),
                        singlecomplex(
                            static_cast<single>(value.real()), static_cast<single>(value.imag())));
                }
            }
        }
        sparseResult->setFromTriplets(triplets.begin(), triplets.end());
        return sparseMatrixPowerResult<singlecomplex>(NLS_SCOMPLEX, dimensions, sparseResult);
    }
    auto* sparseResult
        = new NelsonSparseMatrix<doublecomplex>(dimensions.rows(), dimensions.columns());
    std::vector<Eigen::Triplet<doublecomplex, signedIndexType>> triplets;
    triplets.reserve(static_cast<size_t>(dimensions.elementCount()));
    for (indexType col = 0; col < dimensions.columns(); ++col) {
        for (indexType row = 0; row < dimensions.rows(); ++row) {
            doublecomplex value
                = denseMatrix(static_cast<signedIndexType>(row), static_cast<signedIndexType>(col));
            if (value != doublecomplex(0, 0)) {
                triplets.emplace_back(
                    static_cast<signedIndexType>(row), static_cast<signedIndexType>(col), value);
            }
        }
    }
    sparseResult->setFromTriplets(triplets.begin(), triplets.end());
    return sparseMatrixPowerResult<doublecomplex>(NLS_DCOMPLEX, dimensions, sparseResult);
}
//=============================================================================
static ArrayOf
denseComplexMatrixToArrayOf(
    NelsonType realClass, NelsonType complexClass, const Eigen::MatrixXcd& denseMatrix)
{
    Dimensions dimensions(denseMatrix.rows(), denseMatrix.cols());
    if (complexClass == NLS_SCOMPLEX) {
        auto* ptrC = static_cast<singlecomplex*>(
            ArrayOf::allocateArrayOf(NLS_SCOMPLEX, dimensions.elementCount()));
        for (indexType col = 0; col < dimensions.columns(); ++col) {
            for (indexType row = 0; row < dimensions.rows(); ++row) {
                doublecomplex value = denseMatrix(
                    static_cast<signedIndexType>(row), static_cast<signedIndexType>(col));
                ptrC[row + col * dimensions.rows()] = singlecomplex(
                    static_cast<single>(value.real()), static_cast<single>(value.imag()));
            }
        }
        ArrayOf result(NLS_SCOMPLEX, dimensions, ptrC, false);
        if (result.allReal()) {
            result.promoteType(realClass);
        }
        return result;
    }
    auto* ptrC = static_cast<doublecomplex*>(
        ArrayOf::allocateArrayOf(NLS_DCOMPLEX, dimensions.elementCount()));
    for (indexType col = 0; col < dimensions.columns(); ++col) {
        for (indexType row = 0; row < dimensions.rows(); ++row) {
            ptrC[row + col * dimensions.rows()]
                = denseMatrix(static_cast<signedIndexType>(row), static_cast<signedIndexType>(col));
        }
    }
    ArrayOf result(NLS_DCOMPLEX, dimensions, ptrC, false);
    if (result.allReal()) {
        result.promoteType(realClass);
    }
    return result;
}
//=============================================================================
static doublecomplex
getScalarBaseValue(const ArrayOf& A)
{
    if (A.isComplex()) {
        ArrayOf AA = A;
        AA.promoteType(NLS_DCOMPLEX);
        return reinterpret_cast<const doublecomplex*>((double*)AA.dataPointer())[0];
    }
    ArrayOf AA = A;
    AA.promoteType(NLS_DOUBLE);
    return doublecomplex(static_cast<const double*>(AA.dataPointer())[0], 0);
}
//=============================================================================
template <class T>
static ArrayOf
sparseFractionalMatrixPower(NelsonType realClass, NelsonType complexClass, const ArrayOf& A,
    double exponent, bool& needToOverload)
{
    needToOverload = false;
    Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> denseA = sparseToDenseEigenMatrix<T>(A);
    Eigen::ComplexEigenSolver<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> solver(denseA);
    if (solver.info() != Eigen::Success) {
        Error(L"Nelson:mpower:sparsePowerFailed", L"%s failed.", L"Sparse matrix power");
    }
    Eigen::VectorXcd eigenvalues = solver.eigenvalues().template cast<doublecomplex>();
    Eigen::MatrixXcd eigenvectors = solver.eigenvectors().template cast<doublecomplex>();
    Eigen::VectorXcd powers(eigenvalues.size());
    for (signedIndexType k = 0; k < eigenvalues.size(); ++k) {
        powers(k) = std::pow(eigenvalues(k), exponent);
    }
    Eigen::MatrixXcd result = eigenvectors * powers.asDiagonal() * eigenvectors.inverse();
    return denseComplexMatrixToSparseArrayOf(realClass, complexClass, A.dimensions(), result);
}
//=============================================================================
template <class T>
static ArrayOf
scalarSparseMatrixPower(NelsonType realClass, NelsonType complexClass, const ArrayOf& A,
    const ArrayOf& B, bool& needToOverload)
{
    needToOverload = false;
    if (!A.isScalar() || !B.isSparse()) {
        needToOverload = true;
        return {};
    }
    if (B.rows() != B.columns()) {
        Error(L"Nelson:mpower:scalarOrSquareExpected",
            L"Power (^) operator can only be applied to scalar and square arguments.");
    }
    Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> denseB = sparseToDenseEigenMatrix<T>(B);
    Eigen::ComplexEigenSolver<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> solver(denseB);
    if (solver.info() != Eigen::Success) {
        Error(L"Nelson:mpower:sparsePowerFailed", L"%s failed.", L"Sparse matrix power");
    }
    doublecomplex base = getScalarBaseValue(A);
    Eigen::VectorXcd eigenvalues = solver.eigenvalues().template cast<doublecomplex>();
    Eigen::MatrixXcd eigenvectors = solver.eigenvectors().template cast<doublecomplex>();
    Eigen::VectorXcd powers(eigenvalues.size());
    for (signedIndexType k = 0; k < eigenvalues.size(); ++k) {
        powers(k) = std::pow(base, eigenvalues(k));
    }
    Eigen::MatrixXcd result = eigenvectors * powers.asDiagonal() * eigenvectors.inverse();
    for (signedIndexType col = 0; col < result.cols(); ++col) {
        for (signedIndexType row = 0; row < result.rows(); ++row) {
            if (std::abs(result(row, col).imag()) < 1e-12) {
                result(row, col).imag(0);
            }
        }
    }
    return denseComplexMatrixToArrayOf(realClass, complexClass, result);
}
//=============================================================================
static ArrayOf
scalarSparseMatrixPower(const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    switch (B.dataClass()) {
    case NLS_SINGLE:
        return scalarSparseMatrixPower<single>(NLS_SINGLE, NLS_SCOMPLEX, A, B, needToOverload);
    case NLS_DOUBLE:
        return scalarSparseMatrixPower<double>(NLS_DOUBLE, NLS_DCOMPLEX, A, B, needToOverload);
    case NLS_SCOMPLEX:
        return scalarSparseMatrixPower<singlecomplex>(
            NLS_SINGLE, NLS_SCOMPLEX, A, B, needToOverload);
    case NLS_DCOMPLEX:
        return scalarSparseMatrixPower<doublecomplex>(
            NLS_DOUBLE, NLS_DCOMPLEX, A, B, needToOverload);
    default:
        needToOverload = true;
        return {};
    }
}
//=============================================================================
template <class T>
static ArrayOf
sparseIdentity(NelsonType dataClass, indexType rows, indexType cols)
{
    auto* sparseResult = new NelsonSparseMatrix<T>(rows, cols);
    sparseResult->setIdentity();
    return sparseMatrixPowerResult<T>(dataClass, Dimensions(rows, cols), sparseResult);
}
//=============================================================================
template <class T>
static NelsonSparseMatrix<T>
multiplySparsePowerFactors(const NelsonSparseMatrix<T>& left, const NelsonSparseMatrix<T>& right)
{
    NelsonSparseMatrix<T> product = left * right;
    if constexpr (!std::is_same_v<T, singlecomplex> && !std::is_same_v<T, doublecomplex>) {
        product.prune(T(0));
    }
    product.makeCompressed();
    return product;
}
//=============================================================================
template <class T>
static NelsonSparseMatrix<T>
sparsePowerBySquaring(const NelsonSparseMatrix<T>& base, long int exponent)
{
    NelsonSparseMatrix<T> result(base.rows(), base.cols());
    result.setIdentity();
    NelsonSparseMatrix<T> factor(base);
    while (exponent > 0) {
        if ((exponent % 2) != 0) {
            result = multiplySparsePowerFactors<T>(result, factor);
        }
        exponent /= 2;
        if (exponent > 0) {
            factor = multiplySparsePowerFactors<T>(factor, factor);
        }
    }
    return result;
}
//=============================================================================
template <class T>
static ArrayOf
sparseMatrixIntegerPower(
    NelsonType dataClass, const ArrayOf& A, long int exponent, bool& needToOverload)
{
    needToOverload = false;
    if (A.rows() != A.columns()) {
        Error(L"Nelson:mpower:scalarOrSquareExpected",
            L"Power (^) operator can only be applied to scalar and square arguments.");
    }
    if (exponent == 0) {
        return sparseIdentity<T>(dataClass, A.rows(), A.columns());
    }
    const auto* sparseA = getSparseMatrixPointer<T>(A);
    NelsonSparseMatrix<T> base;
    if (exponent < 0) {
        Eigen::SparseLU<NelsonSparseMatrix<T>, Eigen::COLAMDOrdering<signedIndexType>> solver;
        solver.compute(*sparseA);
        if (solver.info() != Eigen::Success) {
            Error(L"Nelson:mpower:sparseInverseFailed", L"%s failed.", L"Sparse matrix inverse");
        }
        NelsonSparseMatrix<T> identity(sparseA->rows(), sparseA->cols());
        identity.setIdentity();
        base = solver.solve(identity);
        if (solver.info() != Eigen::Success) {
            Error(L"Nelson:mpower:sparseInverseFailed", L"%s failed.", L"Sparse matrix inverse");
        }
        exponent = -exponent;
    } else {
        base = *sparseA;
    }
    auto* sparseResult = new NelsonSparseMatrix<T>(sparsePowerBySquaring<T>(base, exponent));
    return sparseMatrixPowerResult<T>(dataClass, A.dimensions(), sparseResult);
}
//=============================================================================
static ArrayOf
sparseComplexMatrixScalarPower(const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    needToOverload = false;
    double value = getScalarPowerValue(B);
    Eigen::MatrixXcd base;
    NelsonType realClass;
    NelsonType complexClass;
    if (A.dataClass() == NLS_SCOMPLEX) {
        base = sparseToDenseEigenMatrix<singlecomplex>(A).template cast<doublecomplex>();
        realClass = NLS_SINGLE;
        complexClass = NLS_SCOMPLEX;
    } else {
        base = sparseToDenseEigenMatrix<doublecomplex>(A);
        realClass = NLS_DOUBLE;
        complexClass = NLS_DCOMPLEX;
    }
    Eigen::MatrixXcd result;
    if (value == rint(value)) {
        long int exponent = static_cast<long int>(value);
        result = Eigen::MatrixXcd::Identity(base.rows(), base.cols());
        if (exponent < 0) {
            base = base.inverse();
            exponent = -exponent;
        }
        while (exponent > 0) {
            if ((exponent % 2) != 0) {
                result = result * base;
            }
            exponent /= 2;
            if (exponent > 0) {
                base = base * base;
            }
        }
    } else {
        Eigen::ComplexEigenSolver<Eigen::MatrixXcd> solver(base);
        if (solver.info() != Eigen::Success) {
            Error(L"Nelson:mpower:sparsePowerFailed", L"%s failed.", L"Sparse matrix power");
        }
        Eigen::VectorXcd eigenvalues = solver.eigenvalues();
        Eigen::MatrixXcd eigenvectors = solver.eigenvectors();
        Eigen::VectorXcd powers(eigenvalues.size());
        for (signedIndexType k = 0; k < eigenvalues.size(); ++k) {
            powers(k) = std::pow(eigenvalues(k), value);
        }
        result = eigenvectors * powers.asDiagonal() * eigenvectors.inverse();
    }
    return denseComplexMatrixToSparseArrayOf(realClass, complexClass, A.dimensions(), result);
}
//=============================================================================
static ArrayOf
sparseMatrixScalarPower(const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    needToOverload = false;
    if (!A.isSparse() || !B.isScalar()) {
        needToOverload = true;
        return {};
    }
    if (!isRealScalarPowerExponent(B)) {
        needToOverload = true;
        return {};
    }
    double value = getScalarPowerValue(B);
    switch (A.dataClass()) {
    case NLS_SINGLE:
        if (value != rint(value)) {
            return sparseFractionalMatrixPower<single>(
                NLS_SINGLE, NLS_SCOMPLEX, A, value, needToOverload);
        }
        return sparseMatrixIntegerPower<single>(
            NLS_SINGLE, A, static_cast<long int>(value), needToOverload);
    case NLS_DOUBLE:
        if (value != rint(value)) {
            return sparseFractionalMatrixPower<double>(
                NLS_DOUBLE, NLS_DCOMPLEX, A, value, needToOverload);
        }
        return sparseMatrixIntegerPower<double>(
            NLS_DOUBLE, A, static_cast<long int>(value), needToOverload);
    case NLS_SCOMPLEX:
        return sparseComplexMatrixScalarPower(A, B, needToOverload);
    case NLS_DCOMPLEX:
        return sparseComplexMatrixScalarPower(A, B, needToOverload);
    default:
        needToOverload = true;
        return {};
    }
}
//=============================================================================
static ArrayOf
matrixScalarPower(const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    std::wstring errorMessage;
    ArrayOf BB = B;
    double value = BB.asDoubleScalar();
    if (value == rint(value)) {
        if (value == 0) {
            Dimensions dimsA = A.dimensions();
            return Eye(dimsA.rows(), dimsA.columns(), A.dataClass());
        }
        if (value == 1) {
            return A;
        }
        ArrayOf res;
        if (value > 0) {
            res = A;
            auto v = (long int)value;
            long int md = v / 2;
            long int mr = v % 2;
            for (long int k = 1; k < md; k++) {
                res = matrixMultiplication(res, A, needToOverload);
            }
            res = matrixMultiplication(res, res, needToOverload);
            if (mr != 0) {
                res = matrixMultiplication(res, A, needToOverload);
            }
        } else {
            ArrayOf invA = A;
            invA = InverseMatrix(invA, needToOverload);
            res = invA;
            auto v = (long int)abs(value);
            long int md = v / 2;
            long int mr = v % 2;
            for (long int k = 1; k < md; k++) {
                res = matrixMultiplication(res, invA, needToOverload);
            }
            res = matrixMultiplication(res, res, needToOverload);
            if (mr != 0) {
                res = matrixMultiplication(res, invA, needToOverload);
            }
        }
        return res;
    }
    ArrayOf V;
    ArrayOf D;
    if (IsSymmetricWithoutSkew(A, needToOverload)) {
        EigenDecompositionFullSymmetric(A, V, D, needToOverload, errorMessage);
    } else {
        EigenDecompositionFullGeneral(A, false, V, D, needToOverload, errorMessage);
    }
    warnIfIllConditionedEigenvectors(V);
    ArrayOf E = D.diagonal(0);
    ArrayOf F = DoPowerTwoArgFunction(E, B);
    ArrayOf G = ArrayOf::diagonalMatrix(F, 0);
    E = matrixMultiplication(V, G, needToOverload);
    return RightDivide(E, V, needToOverload);
}
//=============================================================================
static ArrayOf
scalarMatrixPower(const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    std::wstring errorMessage;
    ArrayOf V;
    ArrayOf D;
    if (IsSymmetricWithoutSkew(B, needToOverload)) {
        EigenDecompositionFullSymmetric(B, V, D, needToOverload, errorMessage);
    } else {
        EigenDecompositionFullGeneral(B, false, V, D, needToOverload, errorMessage);
    }
    warnIfIllConditionedEigenvectors(V);
    ArrayOf E = D.diagonal(0);
    ArrayOf F = DoPowerTwoArgFunction(A, E);
    ArrayOf G = ArrayOf::diagonalMatrix(F, 0);
    E = matrixMultiplication(V, G, needToOverload);
    return RightDivide(E, V, needToOverload);
}
//=============================================================================
ArrayOf
MatrixPower(const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    needToOverload = false;
    if (A.isScalar() && B.isScalar()) {
        return DotPower(A, B, needToOverload);
    }
    if (A.isScalar() && B.isSparse()) {
        return scalarSparseMatrixPower(A, B, needToOverload);
    }
    if (A.isSparse()) {
        return sparseMatrixScalarPower(A, B, needToOverload);
    }
    if (A.dataClass() > NLS_CHAR) {
        needToOverload = true;
        return {};
    }

    if (A.isEmpty() || B.isEmpty()) {
        Dimensions dims;
        if (A.isEmpty()) {
            dims = A.dimensions();
        } else {
            dims = B.dimensions();
        }
        return ArrayOf::empty(dims);
    }

    if (!A.is2D() || !B.is2D()) {
        Error(L"Nelson:mpower:ndimNotSupported",
            L"Cannot apply exponential operation to N-Dimensional arrays.");
    }

    if (B.isReal() && B.isScalar()) {
        ArrayOf C(B);
        C.promoteType(NLS_DOUBLE);
        double value = C.asDoubleScalar();
        if (value == rint(value) && (value == -1)) {
            return InverseMatrix(A, needToOverload);
        }
    }
    if ((A.dimensionLength(0) != A.dimensionLength(1))
        || (B.dimensionLength(0) != B.dimensionLength(1))) {
        Error(L"Nelson:mpower:scalarOrSquareExpected",
            L"Power (^) operator can only be applied to scalar and square arguments.");
    }
    if (!A.isScalar() && !B.isScalar()) {
        Error(L"Nelson:dimagree:sizeMismatchArgumentsPowerOperator",
            L"Size mismatch on arguments to power (^) operator.");
    }

    bool isSupportedType = (A.isSingleClass() || A.isDoubleClass())
        && (B.isSingleClass() || B.isDoubleClass()) && !A.isSparse() && !B.isSparse();
    if (!isSupportedType) {
        needToOverload = true;
        return {};
    }

    ArrayOf res;
    if (B.isScalar()) {
        res = matrixScalarPower(A, B, needToOverload);
    } else {
        res = scalarMatrixPower(A, B, needToOverload);
    }
    if (res.allReal() && !needToOverload) {
        if (res.dataClass() == NLS_SCOMPLEX) {
            res.promoteType(NLS_SINGLE);
        }
        if (res.dataClass() == NLS_DCOMPLEX) {
            res.promoteType(NLS_DOUBLE);
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
