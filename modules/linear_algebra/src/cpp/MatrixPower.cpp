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
#if defined(_MSC_VER)
#pragma warning(push)
#pragma warning(disable : 4305)
#endif
#include <unsupported/Eigen/MatrixFunctions>
#if defined(_MSC_VER)
#pragma warning(pop)
#endif
#include <algorithm>
#include <limits>
#include <type_traits>
#include <vector>
#include "DotPower.hpp"
#include "ComplexTranspose.hpp"
#include "InverseMatrix.hpp"
#include "IsHermitian.hpp"
#include "LinearEquationSolver.hpp"
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
// E / V with the right division solver, without its singular matrix warning.
static ArrayOf
eigenvectorRightDivide(const ArrayOf& E, const ArrayOf& V, bool& needToOverload)
{
    bool isSinglePrecision = E.isSingleClass() || V.isSingleClass();
    bool isComplex = E.isComplex() || V.isComplex();
    NelsonType commonClass = isSinglePrecision ? (isComplex ? NLS_SCOMPLEX : NLS_SINGLE)
                                               : (isComplex ? NLS_DCOMPLEX : NLS_DOUBLE);
    ArrayOf matV = ComplexTranspose(V, needToOverload);
    ArrayOf matE = ComplexTranspose(E, needToOverload);
    matV.promoteType(commonClass);
    matE.promoteType(commonClass);
    std::wstring warningId;
    std::string warningMessage;
    ArrayOf solved;
    switch (commonClass) {
    case NLS_SINGLE:
        solved = solveLinearEquationSingle(matV, matE, warningId, warningMessage, true);
        break;
    case NLS_SCOMPLEX:
        solved = solveLinearEquationSingleComplex(matV, matE, warningId, warningMessage, true);
        break;
    case NLS_DCOMPLEX:
        solved = solveLinearEquationDoubleComplex(matV, matE, warningId, warningMessage, true);
        break;
    default:
        solved = solveLinearEquationDouble(matV, matE, warningId, warningMessage, true);
        break;
    }
    return ComplexTranspose(solved, needToOverload);
}
//=============================================================================
// Eigenvectors that are singular to working precision mean a defective matrix.
static bool
hasSingularEigenvectors(const ArrayOf& V)
{
    if (V.isEmpty() || !V.is2D()) {
        return false;
    }
    ArrayOf rcondArray = ReciprocalConditionNumber(V);
    rcondArray.promoteType(NLS_DOUBLE);
    double rcondValue = rcondArray.asDoubleScalar();
    bool isSinglePrecision = V.dataClass() == NLS_SINGLE || V.dataClass() == NLS_SCOMPLEX;
    double epsilon = isSinglePrecision ? (double)std::numeric_limits<single>::epsilon()
                                       : std::numeric_limits<double>::epsilon();
    return rcondValue < epsilon * (double)V.rows();
}
//=============================================================================
static bool
hasNegativeRealEigenvalue(const ArrayOf& D)
{
    ArrayOf eigenvalues = ArrayOf(D).diagonal(0);
    eigenvalues.promoteType(NLS_DCOMPLEX);
    const auto* values = reinterpret_cast<const doublecomplex*>(eigenvalues.dataPointer());
    for (indexType k = 0; k < eigenvalues.elementCount(); ++k) {
        if (values[k].imag() == 0 && values[k].real() < 0) {
            return true;
        }
    }
    return false;
}
//=============================================================================
// Scalar power: real pow on the real axis, a negative base turns by p half turns.
template <class T>
static std::complex<T>
scalarFractionalPower(const std::complex<T>& z, T p)
{
    if (z.imag() == (T)0) {
        if (z.real() >= (T)0) {
            // a negative power is the inverse of the positive one
            T value = p < (T)0 ? (T)1 / std::pow(z.real(), -p) : std::pow(z.real(), p);
            return std::complex<T>(value, (T)0);
        }
        T modulus = std::pow(-z.real(), p);
        T halfTurns = std::fmod(p, (T)2);
        bool isOddHalfTurn = std::fmod(std::abs(halfTurns), (T)1) == (T)0.5;
        T angle = static_cast<T>(3.14159265358979323846) * p;
        T cosine = isOddHalfTurn ? (T)0 : std::cos(angle);
        return std::complex<T>(modulus * cosine, modulus * std::sin(angle));
    }
    return std::pow(z, p);
}
//=============================================================================
// Diagonal and superdiagonal of T^p from the 2x2 formulas (equal or well separated eigenvalues).
template <class T, class ComplexMatrix>
static void
setTriangularPowerBands(const ComplexMatrix& matT, T p, ComplexMatrix& powered)
{
    Eigen::Index n = matT.rows();
    for (Eigen::Index i = 0; i < n; ++i) {
        powered(i, i) = scalarFractionalPower<T>(matT(i, i), p);
    }
    for (Eigen::Index i = 1; i < n; ++i) {
        std::complex<T> a = matT(i - 1, i - 1);
        std::complex<T> b = matT(i, i);
        if (a == b) {
            powered(i - 1, i) = p * scalarFractionalPower<T>(b, p - 1) * matT(i - 1, i);
        } else if (2 * std::abs(a) < std::abs(b) || 2 * std::abs(b) < std::abs(a)) {
            powered(i - 1, i) = (powered(i, i) - powered(i - 1, i - 1)) / (b - a) * matT(i - 1, i);
        }
    }
}
//=============================================================================
// Parlett recurrence above the superdiagonal: used when T is singular and defective.
template <class T, class ComplexMatrix>
static void
parlettTriangularPower(const ComplexMatrix& matT, T p, ComplexMatrix& powered)
{
    Eigen::Index n = matT.rows();
    powered.setZero();
    for (Eigen::Index i = 0; i < n; ++i) {
        powered(i, i) = scalarFractionalPower<T>(matT(i, i), p);
    }
    for (Eigen::Index i = 1; i < n; ++i) {
        std::complex<T> a = matT(i - 1, i - 1);
        std::complex<T> b = matT(i, i);
        powered(i - 1, i) = a == b
            ? p * scalarFractionalPower<T>(b, p - 1) * matT(i - 1, i)
            : (powered(i, i) - powered(i - 1, i - 1)) / (b - a) * matT(i - 1, i);
    }
    for (Eigen::Index gap = 2; gap < n; ++gap) {
        for (Eigen::Index i = 0; i + gap < n; ++i) {
            Eigen::Index j = i + gap;
            std::complex<T> sum = matT(i, j) * (powered(j, j) - powered(i, i));
            for (Eigen::Index k = i + 1; k < j; ++k) {
                sum += powered(i, k) * matT(k, j) - matT(i, k) * powered(k, j);
            }
            powered(i, j) = sum / (matT(j, j) - matT(i, i));
        }
    }
}
//=============================================================================
// T^p for an upper triangular T: integer part by products, fractional part by Schur-Pade.
template <class T, class ComplexMatrix>
static ComplexMatrix
triangularFractionalPower(const ComplexMatrix& matT, T p)
{
    Eigen::Index n = matT.rows();
    ComplexMatrix powered = ComplexMatrix::Zero(n, n);
    Eigen::Index zeroCount = 0;
    for (Eigen::Index i = 0; i < n; ++i) {
        if (matT(i, i) == std::complex<T>((T)0, (T)0)) {
            zeroCount++;
        }
    }
    if (zeroCount > 0 && n > 2) {
        Eigen::FullPivLU<ComplexMatrix> lu(matT);
        if (lu.rank() == n - zeroCount) {
            // semisimple zero eigenvalues: the power exists
            Eigen::MatrixPower<ComplexMatrix> power(matT);
            power.compute(powered, p);
            return powered;
        }
        parlettTriangularPower<T>(matT, p, powered);
        return powered;
    }
    T integerPart = std::trunc(p);
    T fractionalPart = p - integerPart;
    if (zeroCount == 0) {
        Eigen::Block<ComplexMatrix> block(powered, 0, 0, n, n);
        Eigen::MatrixPowerAtomic<ComplexMatrix>(matT, fractionalPart).compute(block);
    }
    setTriangularPowerBands<T>(matT, fractionalPart, powered);
    if (integerPart != 0) {
        ComplexMatrix base = integerPart > 0
            ? ComplexMatrix(matT)
            : ComplexMatrix(matT.template triangularView<Eigen::Upper>().solve(
                ComplexMatrix::Identity(n, n)));
        auto count = static_cast<long long>(std::abs(integerPart));
        ComplexMatrix integerPower = ComplexMatrix::Identity(n, n);
        while (count > 0) {
            if ((count % 2) != 0) {
                integerPower = integerPower * base;
            }
            count /= 2;
            if (count > 0) {
                base = base * base;
            }
        }
        powered = integerPower * powered;
    }
    return powered;
}
//=============================================================================
// Schur-Pade power for defective matrices; real A without negative real eigenvalues stays real.
template <class T>
static ArrayOf
schurFractionalPower(const ArrayOf& A, const ArrayOf& D, double exponent)
{
    using ComplexMatrix = Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>;
    constexpr bool isSinglePrecision = std::is_same_v<T, single>;
    NelsonType realClass = isSinglePrecision ? NLS_SINGLE : NLS_DOUBLE;
    NelsonType complexClass = isSinglePrecision ? NLS_SCOMPLEX : NLS_DCOMPLEX;
    auto n = static_cast<Eigen::Index>(A.rows());
    ArrayOf complexA(A);
    complexA.promoteType(complexClass);
    ComplexMatrix matA = Eigen::Map<const ComplexMatrix>(
        reinterpret_cast<const std::complex<T>*>(complexA.dataPointer()), n, n);
    ComplexMatrix powered(n, n);
    if (!matA.allFinite()) {
        powered.setConstant(std::complex<T>(std::numeric_limits<T>::quiet_NaN(), (T)0));
    } else if (matA.isUpperTriangular(0)) {
        powered = triangularFractionalPower<T>(matA, static_cast<T>(exponent));
    } else {
        Eigen::ComplexSchur<ComplexMatrix> schur(matA);
        const ComplexMatrix& matU = schur.matrixU();
        ComplexMatrix triangularPower
            = triangularFractionalPower<T>(ComplexMatrix(schur.matrixT()), static_cast<T>(exponent));
        powered = matU * triangularPower * matU.adjoint();
    }
    indexType count = A.elementCount();
    if (A.isReal() && !hasNegativeRealEigenvalue(D)) {
        T* realValues = static_cast<T*>(ArrayOf::allocateArrayOf(realClass, count));
        for (indexType k = 0; k < count; ++k) {
            realValues[k] = powered.data()[k].real();
        }
        return ArrayOf(realClass, A.dimensions(), realValues);
    }
    auto* complexValues
        = static_cast<std::complex<T>*>(ArrayOf::allocateArrayOf(complexClass, count));
    std::copy(powered.data(), powered.data() + count, complexValues);
    return ArrayOf(complexClass, A.dimensions(), complexValues);
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
// non-integer powers with a sparse operand are computed on dense copies, like full inputs
static ArrayOf
denseMatrixPower(const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    ArrayOf denseA(A);
    ArrayOf denseB(B);
    if (denseA.isSparse()) {
        denseA.makeDense();
    }
    if (denseB.isSparse()) {
        denseB.makeDense();
    }
    return MatrixPower(denseA, denseB, needToOverload);
}
//=============================================================================
static ArrayOf
scalarSparseMatrixPower(const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    if (B.rows() != B.columns()) {
        Error(L"Nelson:mpower:scalarOrSquareExpected",
            L"Power (^) operator can only be applied to scalar and square arguments.");
    }
    return denseMatrixPower(A, B, needToOverload);
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
    // integer exponent: fractional ones go through denseMatrixPower
    long int exponent = static_cast<long int>(value);
    Eigen::MatrixXcd result = Eigen::MatrixXcd::Identity(base.rows(), base.cols());
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
    if (value != rint(value)) {
        if (A.rows() != A.columns()) {
            Error(L"Nelson:mpower:scalarOrSquareExpected",
                L"Power (^) operator can only be applied to scalar and square arguments.");
        }
        ArrayOf res = denseMatrixPower(A, B, needToOverload);
        if (!needToOverload) {
            res.makeSparse();
        }
        return res;
    }
    switch (A.dataClass()) {
    case NLS_SINGLE:
        return sparseMatrixIntegerPower<single>(
            NLS_SINGLE, A, static_cast<long int>(value), needToOverload);
    case NLS_DOUBLE:
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
    // a complex A promotes B to complex: read its real value when the imaginary part is zero
    bool isRealExponent = isRealScalarPowerExponent(B);
    double value = isRealExponent ? getScalarPowerValue(B) : std::nan("");
    if (isRealExponent && value == rint(value)) {
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
    bool isHermitian = IsHermitianWithoutSkew(A, needToOverload);
    if (isHermitian) {
        EigenDecompositionFullSymmetric(A, V, D, needToOverload, errorMessage);
    } else {
        EigenDecompositionFullGeneral(A, false, V, D, needToOverload, errorMessage);
        if (isRealExponent && hasSingularEigenvectors(V)) {
            return (A.dataClass() == NLS_SINGLE || A.dataClass() == NLS_SCOMPLEX)
                ? schurFractionalPower<single>(A, D, value)
                : schurFractionalPower<double>(A, D, value);
        }
    }
    ArrayOf E = D.diagonal(0);
    ArrayOf F = DoPowerTwoArgFunction(E, B);
    ArrayOf G = ArrayOf::diagonalMatrix(F, 0);
    E = matrixMultiplication(V, G, needToOverload);
    if (isHermitian) {
        // unitary eigenvectors: the inverse is the conjugate transpose
        return matrixMultiplication(E, ComplexTranspose(V, needToOverload), needToOverload);
    }
    return eigenvectorRightDivide(E, V, needToOverload);
}
//=============================================================================
template <class T>
static ArrayOf
keepRealPart(const ArrayOf& complexResult, NelsonType realClass)
{
    const T* values = reinterpret_cast<const T*>(complexResult.dataPointer());
    indexType count = complexResult.dimensions().elementCount();
    T* realValues = static_cast<T*>(ArrayOf::allocateArrayOf(realClass, count));
    for (indexType k = 0; k < count; ++k) {
        realValues[k] = values[2 * k];
    }
    return ArrayOf(realClass, complexResult.dimensions(), realValues);
}
//=============================================================================
static bool
isRealNonNegativeScalar(const ArrayOf& A)
{
    if (!A.isReal()) {
        return false;
    }
    ArrayOf value(A);
    value.promoteType(NLS_DOUBLE);
    return value.asDoubleScalar() >= 0;
}
//=============================================================================
static ArrayOf
scalarMatrixPower(const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    std::wstring errorMessage;
    ArrayOf V;
    ArrayOf D;
    if (IsHermitianWithoutSkew(B, needToOverload)) {
        EigenDecompositionFullSymmetric(B, V, D, needToOverload, errorMessage);
    } else {
        EigenDecompositionFullGeneral(B, false, V, D, needToOverload, errorMessage);
    }
    ArrayOf E = D.diagonal(0);
    ArrayOf F = DoPowerTwoArgFunction(A, E);
    ArrayOf G = ArrayOf::diagonalMatrix(F, 0);
    E = matrixMultiplication(V, G, needToOverload);
    ArrayOf res = eigenvectorRightDivide(E, V, needToOverload);
    // a ^ B is real for a real a >= 0 and a real B: drop the rounding imaginary part.
    if (!needToOverload && res.isComplex() && B.isReal() && isRealNonNegativeScalar(A)) {
        return res.dataClass() == NLS_SCOMPLEX ? keepRealPart<single>(res, NLS_SINGLE)
                                               : keepRealPart<double>(res, NLS_DOUBLE);
    }
    return res;
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
