//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "SparseArpackFallback.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "NewWithException.hpp"
#include "SparseType.hpp"
#include "i18n.hpp"
#include <Eigen/Eigenvalues>
#include <Eigen/Sparse>
#include <Eigen/SVD>
#include <algorithm>
#include <cctype>
#include <cmath>
#include <complex>
#include <numeric>
#include <string>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
using SparseDoubleMatrix = Eigen::SparseMatrix<double, 0, signedIndexType>;
using SparseComplexMatrix = Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>;
static constexpr indexType MAX_DENSE_FALLBACK_ELEMENTS = 512 * 512;
//=============================================================================
static std::string
toLowerAscii(std::string value)
{
    std::transform(value.begin(), value.end(), value.begin(),
        [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
    return value;
}
//=============================================================================
static indexType
getK(const ArrayOfVector& argIn, indexType limit)
{
    if (limit < 2) {
        Error(L"Nelson:eigs:matrixDimensionTooSmall",
            L"Input matrix dimension must be greater than one.");
    }
    indexType k = std::min<indexType>(6, limit - 1);
    if (argIn.size() > 1) {
        k = argIn[1].asScalarIndex(true);
    }
    if (k < 1 || k >= limit) {
        Error(L"Nelson:eigs:invalidK",
            L"K must be a positive integer smaller than the matrix dimension.");
    }
    return k;
}
//=============================================================================
static std::string
getWhich(const ArrayOfVector& argIn)
{
    if (argIn.size() < 3) {
        return "lm";
    }
    ArrayOf which(argIn[2]);
    if (!(which.isRowVectorCharacterArray() || which.isScalarStringArray())) {
        return "";
    }
    std::string value = toLowerAscii(which.asUtf8String());
    if (value == "lm" || value == "sm" || value == "lr" || value == "sr" || value == "li"
        || value == "si" || value == "la" || value == "sa") {
        return value;
    }
    Error(L"Nelson:linear_algebra:unsupportedValue", L"Unsupported %s.", L"WHICH value");
    return "lm";
}
//=============================================================================
static std::string
getSingularWhich(const ArrayOfVector& argIn)
{
    if (argIn.size() < 3) {
        return "lm";
    }
    ArrayOf which(argIn[2]);
    if (!(which.isRowVectorCharacterArray() || which.isScalarStringArray())) {
        Error(L"Nelson:eigs:invalidWhich", L"WHICH must be a string.");
    }
    std::string value = toLowerAscii(which.asUtf8String());
    if (value == "lm" || value == "largest") {
        return "lm";
    }
    if (value == "sm" || value == "smallest") {
        return "sm";
    }
    Error(L"Nelson:linear_algebra:unsupportedValue", L"Unsupported %s.", L"WHICH value");
    return "lm";
}
//=============================================================================
static doublecomplex
getSigma(const ArrayOfVector& argIn, bool& haveSigma)
{
    haveSigma = false;
    if (argIn.size() < 3) {
        return 0.;
    }
    ArrayOf third(argIn[2]);
    if (third.isRowVectorCharacterArray() || third.isScalarStringArray()) {
        return 0.;
    }
    if (!third.isScalar()) {
        Error(L"Nelson:eigs:invalidSigma", L"%s must be a finite real scalar.", L"SIGMA");
    }
    doublecomplex sigma = third.isComplex() ? third.asDoubleComplexScalar()
                                            : doublecomplex(third.asDoubleScalar(), 0.0);
    if (!std::isfinite(sigma.real()) || !std::isfinite(sigma.imag())) {
        Error(L"Nelson:eigs:invalidSigma", L"%s must be a finite real scalar.", L"SIGMA");
    }
    haveSigma = true;
    return sigma;
}
//=============================================================================
static bool
orderEigenvalue(const doublecomplex& left, const doublecomplex& right, const std::string& which)
{
    if (which == "sm") {
        return std::abs(left) < std::abs(right);
    }
    if (which == "lr" || which == "la") {
        return left.real() > right.real();
    }
    if (which == "sr" || which == "sa") {
        return left.real() < right.real();
    }
    if (which == "li") {
        double li = std::abs(left.imag());
        double ri = std::abs(right.imag());
        if (li != ri) {
            return li > ri;
        }
        if (left.imag() != right.imag()) {
            return left.imag() > right.imag();
        }
    }
    if (which == "si") {
        double li = std::abs(left.imag());
        double ri = std::abs(right.imag());
        if (li != ri) {
            return li < ri;
        }
        if (left.imag() != right.imag()) {
            return left.imag() < right.imag();
        }
    }
    double la = std::abs(left);
    double ra = std::abs(right);
    if (la == ra) {
        if (left.real() == right.real()) {
            return left.imag() > right.imag();
        }
        return left.real() > right.real();
    }
    return la > ra;
}
//=============================================================================
static bool
allReal(const std::vector<doublecomplex>& values, double tolerance)
{
    for (const auto& value : values) {
        if (std::abs(value.imag()) > tolerance) {
            return false;
        }
    }
    return true;
}
//=============================================================================
static ArrayOf
makeDoubleColumn(const std::vector<double>& values)
{
    auto* ptr = (double*)new_with_exception<double>(values.size(), false);
    std::copy(values.begin(), values.end(), ptr);
    return ArrayOf(NLS_DOUBLE, Dimensions((indexType)values.size(), 1), ptr);
}
//=============================================================================
static ArrayOf
makeComplexColumn(const std::vector<doublecomplex>& values)
{
    if (allReal(values, 1e-14)) {
        std::vector<double> realValues(values.size());
        for (size_t k = 0; k < values.size(); ++k) {
            realValues[k] = values[k].real();
        }
        return makeDoubleColumn(realValues);
    }
    auto* ptr = (doublecomplex*)new_with_exception<doublecomplex>(values.size(), false);
    std::copy(values.begin(), values.end(), ptr);
    return ArrayOf(NLS_DCOMPLEX, Dimensions((indexType)values.size(), 1), ptr);
}
//=============================================================================
static ArrayOf
makeDoubleDenseMatrix(const Eigen::MatrixXd& mat)
{
    auto* ptr = (double*)new_with_exception<double>((size_t)mat.rows() * mat.cols(), false);
    std::copy(mat.data(), mat.data() + mat.size(), ptr);
    return ArrayOf(NLS_DOUBLE, Dimensions((indexType)mat.rows(), (indexType)mat.cols()), ptr);
}
//=============================================================================
static ArrayOf
makeComplexDenseMatrix(const Eigen::MatrixXcd& mat)
{
    if (mat.imag().cwiseAbs().maxCoeff() <= 1e-14) {
        return makeDoubleDenseMatrix(mat.real());
    }
    auto* ptr
        = (doublecomplex*)new_with_exception<doublecomplex>((size_t)mat.rows() * mat.cols(), false);
    std::copy(mat.data(), mat.data() + mat.size(), ptr);
    return ArrayOf(NLS_DCOMPLEX, Dimensions((indexType)mat.rows(), (indexType)mat.cols()), ptr);
}
//=============================================================================
static ArrayOf
makeDoubleDiagonal(const std::vector<double>& values)
{
    indexType n = (indexType)values.size();
    auto* ptr = (double*)new_with_exception<double>((size_t)n * n, true);
    for (indexType i = 0; i < n; ++i) {
        ptr[i + n * i] = values[i];
    }
    return ArrayOf(NLS_DOUBLE, Dimensions(n, n), ptr);
}
//=============================================================================
static ArrayOf
makeComplexDiagonal(const std::vector<doublecomplex>& values)
{
    if (allReal(values, 1e-14)) {
        std::vector<double> realValues(values.size());
        for (size_t k = 0; k < values.size(); ++k) {
            realValues[k] = values[k].real();
        }
        return makeDoubleDiagonal(realValues);
    }
    indexType n = (indexType)values.size();
    auto* ptr = (doublecomplex*)new_with_exception<doublecomplex>((size_t)n * n, true);
    for (indexType i = 0; i < n; ++i) {
        ptr[i + n * i] = values[i];
    }
    return ArrayOf(NLS_DCOMPLEX, Dimensions(n, n), ptr);
}
//=============================================================================
template <class T>
static bool
isFiniteSparse(const Eigen::SparseMatrix<T, 0, signedIndexType>& mat)
{
    for (int k = 0; k < mat.outerSize(); ++k) {
        for (typename Eigen::SparseMatrix<T, 0, signedIndexType>::InnerIterator it(mat, k); it;
            ++it) {
            if (!std::isfinite(std::real(it.value())) || !std::isfinite(std::imag(it.value()))) {
                return false;
            }
        }
    }
    return true;
}
//=============================================================================
static void
checkFallbackSize(const ArrayOf& A)
{
    if (A.elementCount() > MAX_DENSE_FALLBACK_ELEMENTS) {
        Error(L"Nelson:eigs:notAvailable", L"ARPACK support is not available.");
    }
}
//=============================================================================
static Eigen::MatrixXcd
sparseToComplexDense(const ArrayOf& A)
{
    Eigen::MatrixXcd dense(A.rows(), A.columns());
    dense.setZero();
    if (A.dataClass() == NLS_DCOMPLEX) {
        auto* sparseA = (SparseComplexMatrix*)A.sparseDataPointer();
        if (!isFiniteSparse(*sparseA)) {
            Error(L"Nelson:linear_algebra:inputMustBeFinite", L"%s must not contain NaN or Inf.",
                L"Input argument");
        }
        for (int col = 0; col < sparseA->outerSize(); ++col) {
            for (SparseComplexMatrix::InnerIterator it(*sparseA, col); it; ++it) {
                dense(it.row(), it.col()) = it.value();
            }
        }
    } else {
        auto* sparseA = (SparseDoubleMatrix*)A.sparseDataPointer();
        if (!isFiniteSparse(*sparseA)) {
            Error(L"Nelson:linear_algebra:inputMustBeFinite", L"%s must not contain NaN or Inf.",
                L"Input argument");
        }
        for (int col = 0; col < sparseA->outerSize(); ++col) {
            for (SparseDoubleMatrix::InnerIterator it(*sparseA, col); it; ++it) {
                dense(it.row(), it.col()) = it.value();
            }
        }
    }
    return dense;
}
//=============================================================================
ArrayOfVector
EigsSparseDenseFallback(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOf A(argIn[0]);
    if (A.dataClass() != NLS_DOUBLE && A.dataClass() != NLS_DCOMPLEX) {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
            L"sparse double matrix");
    }
    if (!A.is2D() || !A.isSquare()) {
        Error(L"Nelson:linear_algebra:squareMatrixExpected", L"%s expected.", L"Square matrix");
    }
    indexType k = getK(argIn, A.rows());
    bool haveSigma = false;
    doublecomplex sigma = getSigma(argIn, haveSigma);
    std::string which = haveSigma ? "lm" : getWhich(argIn);
    if (A.dataClass() == NLS_DOUBLE && sigma.imag() != 0.0) {
        Error(L"Nelson:eigs:invalidSigma", L"%s must be a finite real scalar.", L"SIGMA");
    }
    checkFallbackSize(A);
    Eigen::MatrixXcd dense = sparseToComplexDense(A);
    Eigen::ComplexEigenSolver<Eigen::MatrixXcd> solver(dense, true);
    if (solver.info() != Eigen::Success) {
        Error(L"Nelson:eigs:solverFailed", L"%s failed.", L"ARPACK eigensolver");
    }
    std::vector<size_t> order((size_t)dense.rows());
    std::iota(order.begin(), order.end(), 0);
    Eigen::VectorXcd eigenvalues = solver.eigenvalues();
    std::sort(
        order.begin(), order.end(), [&eigenvalues, which, haveSigma, sigma](size_t a, size_t b) {
            if (haveSigma) {
                double da = std::abs(eigenvalues((Eigen::Index)a) - sigma);
                double db = std::abs(eigenvalues((Eigen::Index)b) - sigma);
                if (da == db) {
                    return eigenvalues((Eigen::Index)a).real()
                        > eigenvalues((Eigen::Index)b).real();
                }
                return da < db;
            }
            return orderEigenvalue(
                eigenvalues((Eigen::Index)a), eigenvalues((Eigen::Index)b), which);
        });
    std::vector<doublecomplex> values((size_t)k);
    Eigen::MatrixXcd vectors(dense.rows(), (Eigen::Index)k);
    for (indexType i = 0; i < k; ++i) {
        size_t src = order[(size_t)i];
        values[(size_t)i] = eigenvalues((Eigen::Index)src);
        vectors.col((Eigen::Index)i) = solver.eigenvectors().col((Eigen::Index)src);
    }
    ArrayOfVector retval;
    if (nLhs <= 1) {
        retval << makeComplexColumn(values);
    } else {
        retval << makeComplexDenseMatrix(vectors);
        retval << makeComplexDiagonal(values);
    }
    return retval;
}
//=============================================================================
static ArrayOf
makeDenseResult(const Eigen::MatrixXd& mat)
{
    return makeDoubleDenseMatrix(mat);
}
//=============================================================================
static ArrayOf
makeDenseResult(const Eigen::MatrixXcd& mat)
{
    if (mat.size() == 0) {
        return makeDoubleDenseMatrix(Eigen::MatrixXd(mat.rows(), mat.cols()));
    }
    return makeComplexDenseMatrix(mat);
}
//=============================================================================
// k values of an svds call: none means min(6, limit), larger values are clipped
indexType
SvdsRequestedK(const ArrayOfVector& argIn, indexType limit)
{
    indexType k = std::min<indexType>(6, limit);
    if (argIn.size() > 1) {
        const ArrayOf& value = argIn[1];
        double requested = (value.isNumeric() && value.isScalar() && !value.isComplex())
            ? value.asDoubleScalar()
            : -1.0;
        if (!(requested >= 0.0) || std::floor(requested) != requested || std::isinf(requested)) {
            Error(L"Nelson:svds:invalidK", L"K must be a nonnegative integer scalar.");
        }
        k = std::min<indexType>((indexType)requested, limit);
    }
    return k;
}
//=============================================================================
// position of the k singular values to keep, in decreasing order
static std::vector<Eigen::Index>
selectSingularValues(const Eigen::VectorXd& s, indexType k, const ArrayOfVector& argIn)
{
    Eigen::Index n = s.size();
    std::vector<Eigen::Index> order((size_t)n);
    std::iota(order.begin(), order.end(), 0);
    if (argIn.size() > 2 && !argIn[2].isRowVectorCharacterArray()
        && !argIn[2].isScalarStringArray()) {
        if (!argIn[2].isNumeric() || !argIn[2].isScalar() || argIn[2].isComplex()) {
            Error(L"Nelson:eigs:invalidSigma", L"%s must be a finite real scalar.", L"SIGMA");
        }
        double sigma = argIn[2].asDoubleScalar();
        std::stable_sort(order.begin(), order.end(), [&s, sigma](Eigen::Index a, Eigen::Index b) {
            return std::abs(s(a) - sigma) < std::abs(s(b) - sigma);
        });
    } else if (getSingularWhich(argIn) == "sm") {
        std::reverse(order.begin(), order.end());
    }
    order.resize((size_t)k);
    std::sort(order.begin(), order.end());
    return order;
}
//=============================================================================
template <class MatrixType>
static ArrayOfVector
svdsOfDense(int nLhs, const MatrixType& dense, const ArrayOfVector& argIn, indexType k)
{
#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable : 4996)
#else
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"
#endif
    Eigen::BDCSVD<MatrixType> solver(
        dense, nLhs > 1 ? (Eigen::ComputeThinU | Eigen::ComputeThinV) : 0);
#ifdef _MSC_VER
#pragma warning(pop)
#else
#pragma GCC diagnostic pop
#endif
    Eigen::VectorXd s = solver.singularValues();
    std::vector<Eigen::Index> keep = selectSingularValues(s, k, argIn);
    std::vector<double> values(keep.size());
    for (size_t i = 0; i < keep.size(); ++i) {
        values[i] = s(keep[i]);
    }
    ArrayOfVector retval;
    if (nLhs <= 1) {
        retval << makeDoubleColumn(values);
        return retval;
    }
    MatrixType U(dense.rows(), (Eigen::Index)keep.size());
    MatrixType V(dense.cols(), (Eigen::Index)keep.size());
    for (size_t i = 0; i < keep.size(); ++i) {
        U.col((Eigen::Index)i) = solver.matrixU().col(keep[i]);
        V.col((Eigen::Index)i) = solver.matrixV().col(keep[i]);
    }
    retval << makeDenseResult(U);
    retval << makeDoubleDiagonal(values);
    retval << makeDenseResult(V);
    if (nLhs > 3) {
        retval << ArrayOf::doubleScalar(0.0);
    }
    return retval;
}
//=============================================================================
ArrayOfVector
SvdsDense(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOf A(argIn[0]);
    if (A.dataClass() != NLS_DOUBLE && A.dataClass() != NLS_DCOMPLEX) {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
            L"double matrix");
    }
    if (!A.is2D()) {
        Error(L"Nelson:linear_algebra:matrix2DExpected", L"%s expected.", L"Vector or matrix 2D");
    }
    if (A.isSparse()) {
        checkFallbackSize(A);
        A.makeDense();
    }
    indexType k = SvdsRequestedK(argIn, std::min(A.rows(), A.columns()));
    if (A.isEmpty()) {
        // no singular values: empty factors with the input row and column counts
        ArrayOfVector retval;
        retval << makeDoubleColumn(std::vector<double>());
        if (nLhs > 1) {
            retval[0] = makeDoubleDenseMatrix(Eigen::MatrixXd(A.rows(), 0));
            retval << makeDoubleDiagonal(std::vector<double>());
            retval << makeDoubleDenseMatrix(Eigen::MatrixXd(A.columns(), 0));
        }
        if (nLhs > 3) {
            retval << ArrayOf::doubleScalar(0.0);
        }
        return retval;
    }
    if (A.dataClass() == NLS_DCOMPLEX) {
        Eigen::Map<const Eigen::MatrixXcd> dense(
            (const doublecomplex*)A.dataPointer(), A.rows(), A.columns());
        if (!dense.allFinite()) {
            Error(L"Nelson:linear_algebra:inputMustBeFinite", L"%s must not contain NaN or Inf.",
                L"Input argument");
        }
        return svdsOfDense<Eigen::MatrixXcd>(nLhs, dense, argIn, k);
    }
    Eigen::Map<const Eigen::MatrixXd> dense((const double*)A.dataPointer(), A.rows(), A.columns());
    if (!dense.allFinite()) {
        Error(L"Nelson:linear_algebra:inputMustBeFinite", L"%s must not contain NaN or Inf.",
            L"Input argument");
    }
    return svdsOfDense<Eigen::MatrixXd>(nLhs, dense, argIn, k);
}
//=============================================================================
ArrayOfVector
SvdsSparseDenseFallback(int nLhs, const ArrayOfVector& argIn)
{
    return SvdsDense(nLhs, argIn);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
