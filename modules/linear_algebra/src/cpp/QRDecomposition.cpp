//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "QRDecomposition.hpp"
#include "BlasThreadScope.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "SparseArrayOf.hpp"
#include "Types.hpp"
#include "i18n.hpp"
#include "lapack_eigen_config.hpp"
#include "nlsBuildConfig.h"
#undef EIGEN_USE_MKL
#undef EIGEN_USE_MKL_VML
#include "LapackPrototypes.hpp"
#include <Eigen/Dense>
#include <Eigen/SparseQR>
#include <algorithm>
#include <complex>
#include <cstring>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T> using DenseMatrix = Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>;
template <class T> using SparseMatrix = Eigen::SparseMatrix<T, 0, signedIndexType>;
//=============================================================================
template <class T>
static const SparseMatrix<T>*
getSparseMatrixPointer(const ArrayOf& A)
{
    return static_cast<const SparseMatrix<T>*>(A.sparseDataPointer());
}
//=============================================================================
static NelsonType
permutationClass(NelsonType dataClass)
{
    return dataClass == NLS_SINGLE || dataClass == NLS_SCOMPLEX ? NLS_SINGLE : NLS_DOUBLE;
}
//=============================================================================
template <class T>
static ArrayOf
denseMatrixToArray(NelsonType dataClass, const DenseMatrix<T>& matrix)
{
    Dimensions dimensions((indexType)matrix.rows(), (indexType)matrix.cols());
    indexType length = dimensions.elementCount();
    if (length == 0) {
        return ArrayOf(dataClass, dimensions, nullptr);
    }
    T* data = (T*)ArrayOf::allocateArrayOf(dataClass, length, stringVector(), false);
    std::copy(matrix.data(), matrix.data() + length, data);
    return ArrayOf(dataClass, dimensions, data);
}
//=============================================================================
template <class T>
static ArrayOf
sparseMatrixToArray(NelsonType dataClass, const SparseMatrix<T>& matrix)
{
    return makeSparseArrayOfFromEigen<T>(dataClass,
        Dimensions((indexType)matrix.rows(), (indexType)matrix.cols()), new SparseMatrix<T>(matrix),
        true);
}
//=============================================================================
template <class T, class Permutation>
static ArrayOf
makeSparsePermutationMatrix(NelsonType dataClass, const Permutation& permutation, indexType n)
{
    using PType = typename Eigen::NumTraits<T>::Real;
    using Triplet = Eigen::Triplet<PType, signedIndexType>;
    std::vector<Triplet> triplets;
    triplets.reserve((size_t)n);
    for (signedIndexType row = 0; row < permutation.indices().size(); ++row) {
        triplets.emplace_back(row, permutation.indices()[row], PType(1));
    }
    SparseMatrix<PType> P((Eigen::Index)n, (Eigen::Index)n);
    P.setFromTriplets(triplets.begin(), triplets.end());
    return sparseMatrixToArray<PType>(permutationClass(dataClass), P);
}
//=============================================================================
template <class T>
static DenseMatrix<T>
arrayToDenseMatrix(const ArrayOf& A)
{
    DenseMatrix<T> matrix((Eigen::Index)A.rows(), (Eigen::Index)A.columns());
    if (A.isSparse()) {
        matrix.setZero();
        const auto* sparseA = getSparseMatrixPointer<T>(A);
        for (signedIndexType col = 0; col < sparseA->outerSize(); ++col) {
            for (typename SparseMatrix<T>::InnerIterator it(*sparseA, col); it; ++it) {
                matrix(it.row(), it.col()) = it.value();
            }
        }
        return matrix;
    }
    const auto* data = reinterpret_cast<const T*>(A.dataPointer());
    Eigen::Map<const DenseMatrix<T>> input(data, (Eigen::Index)A.rows(), (Eigen::Index)A.columns());
    matrix = input;
    return matrix;
}
//=============================================================================
template <class T>
static ArrayOf
makePermutationVector(const std::vector<lapack_int>& jpvt)
{
    Dimensions dimensions(1, (indexType)jpvt.size());
    if (jpvt.empty()) {
        return ArrayOf(NLS_DOUBLE, dimensions, nullptr);
    }
    double* data = (double*)ArrayOf::allocateArrayOf(
        NLS_DOUBLE, (indexType)jpvt.size(), stringVector(), false);
    for (size_t k = 0; k < jpvt.size(); ++k) {
        data[k] = (double)jpvt[k];
    }
    return ArrayOf(NLS_DOUBLE, dimensions, data);
}
//=============================================================================
template <class T>
static ArrayOf
makePermutationMatrix(NelsonType dataClass, const std::vector<lapack_int>& jpvt)
{
    NelsonType pClass = permutationClass(dataClass);
    indexType n = (indexType)jpvt.size();
    Dimensions dimensions(n, n);
    if (n == 0) {
        return ArrayOf(pClass, dimensions, nullptr);
    }
    using PType = typename Eigen::NumTraits<T>::Real;
    PType* data = (PType*)ArrayOf::allocateArrayOf(pClass, n * n, stringVector(), true);
    for (indexType col = 0; col < n; ++col) {
        indexType row = (indexType)jpvt[(size_t)col] - 1;
        data[row + col * n] = PType(1);
    }
    return ArrayOf(pClass, dimensions, data);
}
//=============================================================================
template <class T>
static T*
makeScalarValue(NelsonType dataClass, T value)
{
    T* data = (T*)ArrayOf::allocateArrayOf(dataClass, 1, stringVector(), false);
    data[0] = value;
    return data;
}
//=============================================================================
static ArrayOf
makeScalarOne(NelsonType dataClass)
{
    switch (dataClass) {
    case NLS_DOUBLE:
        return ArrayOf(dataClass, Dimensions(1, 1), makeScalarValue<double>(dataClass, 1.));
    case NLS_SINGLE:
        return ArrayOf(dataClass, Dimensions(1, 1), makeScalarValue<single>(dataClass, (single)1));
    case NLS_DCOMPLEX:
        return ArrayOf(dataClass, Dimensions(1, 1),
            makeScalarValue<doublecomplex>(dataClass, doublecomplex(1., 0.)));
    case NLS_SCOMPLEX:
        return ArrayOf(dataClass, Dimensions(1, 1),
            makeScalarValue<singlecomplex>(dataClass, singlecomplex((single)1, (single)0)));
    default:
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
            L"single or double");
    }
    return {};
}
//=============================================================================
static ArrayOf
makeScalarPermutation(NelsonType dataClass, QRPermutationOutput permutationOutput)
{
    if (permutationOutput == QRPermutationOutput::Vector) {
        return ArrayOf(NLS_DOUBLE, Dimensions(1, 1), makeScalarValue<double>(NLS_DOUBLE, 1.));
    }
    NelsonType pClass = permutationClass(dataClass);
    if (pClass == NLS_SINGLE) {
        return ArrayOf(pClass, Dimensions(1, 1), makeScalarValue<single>(pClass, (single)1));
    }
    return ArrayOf(pClass, Dimensions(1, 1), makeScalarValue<double>(pClass, 1.));
}
//=============================================================================
static ArrayOfVector
scalarQR(const ArrayOf& A, int nLhs, QRPermutationOutput permutationOutput)
{
    ArrayOfVector retval;
    if (nLhs <= 1) {
        retval << A;
        return retval;
    }
    retval << makeScalarOne(A.dataClass());
    retval << A;
    if (nLhs == 3) {
        retval << makeScalarPermutation(A.dataClass(), permutationOutput);
    }
    return retval;
}
//=============================================================================
template <class T>
static ArrayOf
makeRArray(NelsonType dataClass, const T* factor, lapack_int m, lapack_int n, QRMode mode)
{
    lapack_int rows = (mode == QRMode::Economy && m > n) ? n : m;
    Dimensions dimensions((indexType)rows, (indexType)n);
    if (rows == 0 || n == 0) {
        return ArrayOf(dataClass, dimensions, nullptr);
    }
    T* data = (T*)ArrayOf::allocateArrayOf(
        dataClass, (indexType)rows * (indexType)n, stringVector(), true);
    for (lapack_int col = 0; col < n; ++col) {
        lapack_int lastRow = std::min({ rows, m, col + 1 });
        for (lapack_int row = 0; row < lastRow; ++row) {
            data[(indexType)row + (indexType)col * (indexType)rows]
                = factor[(size_t)(row + col * m)];
        }
    }
    return ArrayOf(dataClass, dimensions, data);
}
//=============================================================================
template <class T>
static void
zeroStrictLowerTriangle(T* data, lapack_int rows, lapack_int columns)
{
    for (lapack_int col = 0; col < columns; ++col) {
        for (lapack_int row = col + 1; row < rows; ++row) {
            data[(indexType)row + (indexType)col * (indexType)rows] = T(0);
        }
    }
}
//=============================================================================
template <class T>
static T*
makeQWorkspaceData(NelsonType dataClass, const std::vector<T>& factor, lapack_int m, lapack_int n,
    QRMode mode, lapack_int& cols)
{
    cols = (mode == QRMode::Economy && m > n) ? n : m;
    if (m == 0 || cols == 0) {
        return nullptr;
    }
    lapack_int k = std::min(m, n);
    bool initializeIdentity = k == 0;
    T* data = (T*)ArrayOf::allocateArrayOf(
        dataClass, (indexType)m * (indexType)cols, stringVector(), initializeIdentity);
    lapack_int copyCols = std::min(cols, n);
    for (lapack_int col = 0; col < copyCols; ++col) {
        for (lapack_int row = 0; row < m; ++row) {
            data[(indexType)row + (indexType)col * (indexType)m] = factor[(size_t)(row + col * m)];
        }
    }
    if (initializeIdentity) {
        for (lapack_int diag = copyCols; diag < std::min(m, cols); ++diag) {
            data[(indexType)diag + (indexType)diag * (indexType)m] = T(1);
        }
    }
    return data;
}
//=============================================================================
template <class T>
static void
checkLapackInfo(lapack_int info, const std::string& routine)
{
    if (info < 0) {
        Error(L"Nelson:qr:lapackError", L"Invalid argument in LAPACK QR routine.");
    }
    if (info > 0) {
        Error(L"Nelson:linear_algebra:factorizationFailed", L"%s failed.", L"QR decomposition");
    }
}
//=============================================================================
static lapack_int
defaultQrWorkspace(lapack_int n)
{
    constexpr lapack_int blockSize = 32;
    return std::max<lapack_int>(1, n * blockSize);
}
//=============================================================================
static lapack_int
defaultQp3Workspace(lapack_int n)
{
    constexpr lapack_int blockSize = 16;
    return std::max<lapack_int>(1, 2 * n + (n + 1) * blockSize);
}
//=============================================================================
static lapack_int
lapackDgeqrfWorkspace(int, lapack_int m, lapack_int n, double* a, lapack_int lda, double* tau)
{
    int M = (int)m;
    int N = (int)n;
    int LDA = (int)lda;
    int INFO = 0;
    int LWORK = (int)defaultQrWorkspace(n);
    std::vector<double> work((size_t)LWORK);
    LAPACK_dgeqrf(&M, &N, a, &LDA, tau, work.data(), &LWORK, &INFO);
    return INFO;
}
//=============================================================================
static lapack_int
lapackSgeqrfWorkspace(int, lapack_int m, lapack_int n, single* a, lapack_int lda, single* tau)
{
    lapack_int info = 0;
    lapack_int lwork = defaultQrWorkspace(n);
    std::vector<single> work((size_t)lwork);
    LAPACK_sgeqrf(&m, &n, a, &lda, tau, work.data(), &lwork, &info);
    return info;
}
//=============================================================================
static lapack_int
lapackCgeqrfWorkspace(
    int, lapack_int m, lapack_int n, singlecomplex* a, lapack_int lda, singlecomplex* tau)
{
    lapack_int info = 0;
    lapack_int lwork = defaultQrWorkspace(n);
    std::vector<singlecomplex> work((size_t)lwork);
    LAPACK_cgeqrf(&m, &n, a, &lda, tau, work.data(), &lwork, &info);
    return info;
}
//=============================================================================
static lapack_int
lapackZgeqrfWorkspace(
    int, lapack_int m, lapack_int n, doublecomplex* a, lapack_int lda, doublecomplex* tau)
{
    lapack_int info = 0;
    lapack_int lwork = defaultQrWorkspace(n);
    std::vector<doublecomplex> work((size_t)lwork);
    LAPACK_zgeqrf(&m, &n, a, &lda, tau, work.data(), &lwork, &info);
    return info;
}
//=============================================================================
static lapack_int
lapackDgeqp3Workspace(
    int, lapack_int m, lapack_int n, double* a, lapack_int lda, lapack_int* jpvt, double* tau)
{
    lapack_int info = 0;
    lapack_int lwork = defaultQp3Workspace(n);
    std::vector<double> work((size_t)lwork);
    LAPACK_dgeqp3(&m, &n, a, &lda, jpvt, tau, work.data(), &lwork, &info);
    return info;
}
//=============================================================================
static lapack_int
lapackSgeqp3Workspace(
    int, lapack_int m, lapack_int n, single* a, lapack_int lda, lapack_int* jpvt, single* tau)
{
    lapack_int info = 0;
    lapack_int lwork = defaultQp3Workspace(n);
    std::vector<single> work((size_t)lwork);
    LAPACK_sgeqp3(&m, &n, a, &lda, jpvt, tau, work.data(), &lwork, &info);
    return info;
}
//=============================================================================
static lapack_int
lapackCgeqp3Workspace(int, lapack_int m, lapack_int n, singlecomplex* a, lapack_int lda,
    lapack_int* jpvt, singlecomplex* tau)
{
    lapack_int info = 0;
    std::vector<single> rwork((size_t)std::max<lapack_int>(1, 2 * n));
    lapack_int lwork = defaultQp3Workspace(n);
    std::vector<singlecomplex> work((size_t)lwork);
    LAPACK_cgeqp3(&m, &n, a, &lda, jpvt, tau, work.data(), &lwork, rwork.data(), &info);
    return info;
}
//=============================================================================
static lapack_int
lapackZgeqp3Workspace(int, lapack_int m, lapack_int n, doublecomplex* a, lapack_int lda,
    lapack_int* jpvt, doublecomplex* tau)
{
    lapack_int info = 0;
    std::vector<double> rwork((size_t)std::max<lapack_int>(1, 2 * n));
    lapack_int lwork = defaultQp3Workspace(n);
    std::vector<doublecomplex> work((size_t)lwork);
    LAPACK_zgeqp3(&m, &n, a, &lda, jpvt, tau, work.data(), &lwork, rwork.data(), &info);
    return info;
}
//=============================================================================
static lapack_int
lapackDorgqrWorkspace(
    int, lapack_int m, lapack_int n, lapack_int k, double* a, lapack_int lda, const double* tau)
{
    int M = (int)m;
    int N = (int)n;
    int K = (int)k;
    int LDA = (int)lda;
    int INFO = 0;
    int LWORK = (int)defaultQrWorkspace(n);
    std::vector<double> work((size_t)LWORK);
    LAPACK_dorgqr(&M, &N, &K, a, &LDA, tau, work.data(), &LWORK, &INFO);
    return INFO;
}
//=============================================================================
static lapack_int
lapackSorgqrWorkspace(
    int, lapack_int m, lapack_int n, lapack_int k, single* a, lapack_int lda, const single* tau)
{
    lapack_int info = 0;
    lapack_int lwork = defaultQrWorkspace(n);
    std::vector<single> work((size_t)lwork);
    LAPACK_sorgqr(&m, &n, &k, a, &lda, tau, work.data(), &lwork, &info);
    return info;
}
//=============================================================================
static lapack_int
lapackCungqrWorkspace(int, lapack_int m, lapack_int n, lapack_int k, singlecomplex* a,
    lapack_int lda, const singlecomplex* tau)
{
    lapack_int info = 0;
    lapack_int lwork = defaultQrWorkspace(n);
    std::vector<singlecomplex> work((size_t)lwork);
    LAPACK_cungqr(&m, &n, &k, a, &lda, tau, work.data(), &lwork, &info);
    return info;
}
//=============================================================================
static lapack_int
lapackZungqrWorkspace(int, lapack_int m, lapack_int n, lapack_int k, doublecomplex* a,
    lapack_int lda, const doublecomplex* tau)
{
    lapack_int info = 0;
    lapack_int lwork = defaultQrWorkspace(n);
    std::vector<doublecomplex> work((size_t)lwork);
    LAPACK_zungqr(&m, &n, &k, a, &lda, tau, work.data(), &lwork, &info);
    return info;
}
//=============================================================================
template <class T>
static ArrayOfVector
denseRealQR(const ArrayOf& A, int nLhs, QRMode mode, QRPermutationOutput permutationOutput,
    lapack_int (*geqrf)(int, lapack_int, lapack_int, T*, lapack_int, T*),
    lapack_int (*geqp3)(int, lapack_int, lapack_int, T*, lapack_int, lapack_int*, T*),
    lapack_int (*orgqr)(int, lapack_int, lapack_int, lapack_int, T*, lapack_int, const T*))
{
    lapack_int m = (lapack_int)A.rows();
    lapack_int n = (lapack_int)A.columns();
    lapack_int k = std::min(m, n);
    bool pivoted = nLhs == 3;
    BlasThreadScope blasThreadScope(useSingleThreadBlasForSmallMatrix(m, n), pivoted ? 1 : 5);
    const T* input = reinterpret_cast<const T*>(A.dataPointer());
    if (!pivoted && nLhs <= 1 && mode == QRMode::Full) {
        Dimensions dimensions((indexType)m, (indexType)n);
        T* data = nullptr;
        if (m > 0 && n > 0) {
            data = (T*)ArrayOf::allocateArrayOf(
                A.dataClass(), (indexType)m * (indexType)n, stringVector(), false);
            std::memcpy(data, input, (size_t)m * (size_t)n * sizeof(T));
            std::vector<T> tau((size_t)k);
            lapack_int info = geqrf(0, m, n, data, m, tau.data());
            checkLapackInfo<T>(info, "geqrf");
            zeroStrictLowerTriangle<T>(data, m, n);
        }
        ArrayOfVector retval;
        retval << ArrayOf(A.dataClass(), dimensions, data);
        return retval;
    }
    std::vector<T> factor(input, input + (size_t)m * (size_t)n);
    std::vector<T> tau((size_t)k);
    std::vector<lapack_int> jpvt((size_t)n, 0);
    if (pivoted && k == 0) {
        for (lapack_int col = 0; col < n; ++col) {
            jpvt[(size_t)col] = col + 1;
        }
    }
    if (k > 0) {
        lapack_int info = pivoted ? geqp3(0, m, n, factor.data(), m, jpvt.data(), tau.data())
                                  : geqrf(0, m, n, factor.data(), m, tau.data());
        checkLapackInfo<T>(info, "geqrf");
    }

    ArrayOfVector retval;
    ArrayOf R = makeRArray<T>(A.dataClass(), factor.data(), m, n, mode);
    if (nLhs <= 1) {
        retval << R;
        return retval;
    }

    lapack_int qCols = 0;
    T* qData = makeQWorkspaceData<T>(A.dataClass(), factor, m, n, mode, qCols);
    if (k > 0 && qCols > 0) {
        lapack_int info = orgqr(0, m, qCols, k, qData, m, tau.data());
        checkLapackInfo<T>(info, "orgqr");
    }
    retval << ArrayOf(A.dataClass(), Dimensions((indexType)m, (indexType)qCols), qData);
    retval << R;
    if (nLhs == 3) {
        if (permutationOutput == QRPermutationOutput::Vector) {
            retval << makePermutationVector<T>(jpvt);
        } else {
            retval << makePermutationMatrix<T>(A.dataClass(), jpvt);
        }
    }
    return retval;
}
//=============================================================================
template <class T>
static ArrayOfVector
denseComplexQR(const ArrayOf& A, int nLhs, QRMode mode, QRPermutationOutput permutationOutput,
    lapack_int (*geqrf)(
        int, lapack_int, lapack_int, std::complex<T>*, lapack_int, std::complex<T>*),
    lapack_int (*geqp3)(
        int, lapack_int, lapack_int, std::complex<T>*, lapack_int, lapack_int*, std::complex<T>*),
    lapack_int (*ungqr)(int, lapack_int, lapack_int, lapack_int, std::complex<T>*, lapack_int,
        const std::complex<T>*))
{
    using Z = std::complex<T>;
    lapack_int m = (lapack_int)A.rows();
    lapack_int n = (lapack_int)A.columns();
    lapack_int k = std::min(m, n);
    bool pivoted = nLhs == 3;
    BlasThreadScope blasThreadScope(useSingleThreadBlasForSmallMatrix(m, n), pivoted ? 1 : 5);
    const Z* input = reinterpret_cast<const Z*>(A.dataPointer());
    if (!pivoted && nLhs <= 1 && mode == QRMode::Full) {
        Dimensions dimensions((indexType)m, (indexType)n);
        Z* data = nullptr;
        if (m > 0 && n > 0) {
            data = (Z*)ArrayOf::allocateArrayOf(
                A.dataClass(), (indexType)m * (indexType)n, stringVector(), false);
            std::memcpy(data, input, (size_t)m * (size_t)n * sizeof(Z));
            std::vector<Z> tau((size_t)k);
            lapack_int info = geqrf(0, m, n, data, m, tau.data());
            checkLapackInfo<T>(info, "geqrf");
            zeroStrictLowerTriangle<Z>(data, m, n);
        }
        ArrayOfVector retval;
        retval << ArrayOf(A.dataClass(), dimensions, data);
        return retval;
    }
    std::vector<Z> factor(input, input + (size_t)m * (size_t)n);
    std::vector<Z> tau((size_t)k);
    std::vector<lapack_int> jpvt((size_t)n, 0);
    if (pivoted && k == 0) {
        for (lapack_int col = 0; col < n; ++col) {
            jpvt[(size_t)col] = col + 1;
        }
    }
    if (k > 0) {
        lapack_int info = pivoted ? geqp3(0, m, n, factor.data(), m, jpvt.data(), tau.data())
                                  : geqrf(0, m, n, factor.data(), m, tau.data());
        checkLapackInfo<T>(info, "geqrf");
    }

    ArrayOfVector retval;
    ArrayOf R = makeRArray<Z>(A.dataClass(), factor.data(), m, n, mode);
    if (nLhs <= 1) {
        retval << R;
        return retval;
    }

    lapack_int qCols = 0;
    Z* qData = makeQWorkspaceData<Z>(A.dataClass(), factor, m, n, mode, qCols);
    if (k > 0 && qCols > 0) {
        lapack_int info = ungqr(0, m, qCols, k, qData, m, tau.data());
        checkLapackInfo<T>(info, "ungqr");
    }
    retval << ArrayOf(A.dataClass(), Dimensions((indexType)m, (indexType)qCols), qData);
    retval << R;
    if (nLhs == 3) {
        if (permutationOutput == QRPermutationOutput::Vector) {
            retval << makePermutationVector<Z>(jpvt);
        } else {
            retval << makePermutationMatrix<Z>(A.dataClass(), jpvt);
        }
    }
    return retval;
}
//=============================================================================
template <class T, class Ordering>
static ArrayOfVector
sparseQRFactors(const ArrayOf& A, int nLhs, QRMode mode, QRPermutationOutput permutationOutput)
{
    const auto* sparseA = getSparseMatrixPointer<T>(A);
    Eigen::SparseQR<SparseMatrix<T>, Ordering> solver;
    solver.compute(*sparseA);
    if (solver.info() != Eigen::Success) {
        Error(L"Nelson:linear_algebra:factorizationFailed", L"%s failed.", L"QR decomposition");
    }

    SparseMatrix<T> Rfull = solver.matrixR();
    indexType rRows = (mode == QRMode::Economy && A.rows() > A.columns()) ? A.columns() : A.rows();
    SparseMatrix<T> R = Rfull.topRows((Eigen::Index)rRows);
    ArrayOfVector retval;
    if (nLhs <= 1) {
        retval << sparseMatrixToArray<T>(A.dataClass(), R);
        return retval;
    }

    DenseMatrix<T> identity
        = DenseMatrix<T>::Identity((Eigen::Index)A.rows(), (Eigen::Index)A.rows());
    DenseMatrix<T> QfullDense = solver.matrixQ() * identity;
    indexType qCols = (mode == QRMode::Economy && A.rows() > A.columns()) ? A.columns() : A.rows();
    SparseMatrix<T> Q = QfullDense.leftCols((Eigen::Index)qCols).sparseView();
    retval << sparseMatrixToArray<T>(A.dataClass(), Q);
    retval << sparseMatrixToArray<T>(A.dataClass(), R);

    if (nLhs == 3) {
        auto permutation = solver.colsPermutation();
        if (permutationOutput == QRPermutationOutput::Vector) {
            std::vector<double> p((size_t)A.columns(), 0);
            for (signedIndexType row = 0; row < permutation.indices().size(); ++row) {
                signedIndexType col = permutation.indices()[row];
                if (col >= 0 && col < (signedIndexType)p.size()) {
                    p[(size_t)col] = (double)row + 1;
                }
            }
            Dimensions dimensions(1, A.columns());
            double* data = nullptr;
            if (!p.empty()) {
                data = (double*)ArrayOf::allocateArrayOf(
                    NLS_DOUBLE, (indexType)p.size(), stringVector(), false);
                std::copy(p.begin(), p.end(), data);
            }
            retval << ArrayOf(NLS_DOUBLE, dimensions, data);
        } else {
            retval << makeSparsePermutationMatrix<T>(A.dataClass(), permutation, A.columns());
        }
    }
    return retval;
}
//=============================================================================
template <class T, class Ordering>
static ArrayOfVector
sparseQRLeastSquares(const ArrayOf& S, const ArrayOf& B, int nLhs, QRMode mode,
    QRPermutationOutput permutationOutput)
{
    const auto* sparseS = getSparseMatrixPointer<T>(S);
    Eigen::SparseQR<SparseMatrix<T>, Ordering> solver;
    solver.compute(*sparseS);
    if (solver.info() != Eigen::Success) {
        Error(L"Nelson:linear_algebra:factorizationFailed", L"%s failed.", L"QR decomposition");
    }

    indexType rows = (mode == QRMode::Economy && S.rows() > S.columns()) ? S.columns() : S.rows();
    ArrayOfVector retval;
    if (B.isSparse()) {
        DenseMatrix<T> denseB = arrayToDenseMatrix<T>(B);
        DenseMatrix<T> Cfull = solver.matrixQ().adjoint() * denseB;
        SparseMatrix<T> C = Cfull.topRows((Eigen::Index)rows).sparseView();
        SparseMatrix<T> R = solver.matrixR().topRows((Eigen::Index)rows);
        retval << sparseMatrixToArray<T>(B.dataClass(), C);
        retval << sparseMatrixToArray<T>(S.dataClass(), R);
    } else {
        DenseMatrix<T> denseB = arrayToDenseMatrix<T>(B);
        DenseMatrix<T> Cfull = solver.matrixQ().adjoint() * denseB;
        DenseMatrix<T> C = Cfull.topRows((Eigen::Index)rows);
        SparseMatrix<T> R = solver.matrixR().topRows((Eigen::Index)rows);
        retval << denseMatrixToArray<T>(B.dataClass(), C);
        retval << sparseMatrixToArray<T>(S.dataClass(), R);
    }

    if (nLhs == 3) {
        auto permutation = solver.colsPermutation();
        if (permutationOutput == QRPermutationOutput::Vector) {
            std::vector<double> p((size_t)S.columns(), 0);
            for (signedIndexType row = 0; row < permutation.indices().size(); ++row) {
                signedIndexType col = permutation.indices()[row];
                if (col >= 0 && col < (signedIndexType)p.size()) {
                    p[(size_t)col] = (double)row + 1;
                }
            }
            Dimensions dimensions(1, S.columns());
            double* data = nullptr;
            if (!p.empty()) {
                data = (double*)ArrayOf::allocateArrayOf(
                    NLS_DOUBLE, (indexType)p.size(), stringVector(), false);
                std::copy(p.begin(), p.end(), data);
            }
            retval << ArrayOf(NLS_DOUBLE, dimensions, data);
        } else {
            retval << makeSparsePermutationMatrix<T>(S.dataClass(), permutation, S.columns());
        }
    }
    return retval;
}
//=============================================================================
ArrayOfVector
QRDecomposition(const ArrayOf& A, int nLhs, QRMode mode, QRPermutationOutput permutationOutput)
{
    if (!A.isSparse() && A.rows() == 1 && A.columns() == 1) {
        return scalarQR(A, nLhs, permutationOutput);
    }
    if (A.isSparse()) {
        switch (A.dataClass()) {
        case NLS_DOUBLE:
            if (nLhs == 3) {
                return sparseQRFactors<double, Eigen::COLAMDOrdering<signedIndexType>>(
                    A, nLhs, mode, permutationOutput);
            }
            return sparseQRFactors<double, Eigen::NaturalOrdering<signedIndexType>>(
                A, nLhs, mode, permutationOutput);
        case NLS_SINGLE:
            if (nLhs == 3) {
                return sparseQRFactors<single, Eigen::COLAMDOrdering<signedIndexType>>(
                    A, nLhs, mode, permutationOutput);
            }
            return sparseQRFactors<single, Eigen::NaturalOrdering<signedIndexType>>(
                A, nLhs, mode, permutationOutput);
        case NLS_DCOMPLEX:
            if (nLhs == 3) {
                return sparseQRFactors<doublecomplex, Eigen::COLAMDOrdering<signedIndexType>>(
                    A, nLhs, mode, permutationOutput);
            }
            return sparseQRFactors<doublecomplex, Eigen::NaturalOrdering<signedIndexType>>(
                A, nLhs, mode, permutationOutput);
        case NLS_SCOMPLEX:
            if (nLhs == 3) {
                return sparseQRFactors<singlecomplex, Eigen::COLAMDOrdering<signedIndexType>>(
                    A, nLhs, mode, permutationOutput);
            }
            return sparseQRFactors<singlecomplex, Eigen::NaturalOrdering<signedIndexType>>(
                A, nLhs, mode, permutationOutput);
        default:
            Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
                L"single or double");
        }
    }

    switch (A.dataClass()) {
    case NLS_DOUBLE:
        return denseRealQR<double>(A, nLhs, mode, permutationOutput, lapackDgeqrfWorkspace,
            lapackDgeqp3Workspace, lapackDorgqrWorkspace);
    case NLS_SINGLE:
        return denseRealQR<single>(A, nLhs, mode, permutationOutput, lapackSgeqrfWorkspace,
            lapackSgeqp3Workspace, lapackSorgqrWorkspace);
    case NLS_DCOMPLEX:
        return denseComplexQR<double>(A, nLhs, mode, permutationOutput, lapackZgeqrfWorkspace,
            lapackZgeqp3Workspace, lapackZungqrWorkspace);
    case NLS_SCOMPLEX:
        return denseComplexQR<single>(A, nLhs, mode, permutationOutput, lapackCgeqrfWorkspace,
            lapackCgeqp3Workspace, lapackCungqrWorkspace);
    default:
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
            L"single or double");
    }
    return {};
}
//=============================================================================
ArrayOfVector
QRSparseLeastSquares(const ArrayOf& S, const ArrayOf& B, int nLhs, QRMode mode,
    QRPermutationOutput permutationOutput)
{
    if (!S.isSparse()) {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
            L"sparse matrix");
    }
    if (S.dataClass() != B.dataClass()) {
        Error(L"Nelson:types:sameTypeExpected", L"Input arguments must have the same type.");
    }
    switch (S.dataClass()) {
    case NLS_DOUBLE:
        if (nLhs == 3) {
            return sparseQRLeastSquares<double, Eigen::COLAMDOrdering<signedIndexType>>(
                S, B, nLhs, mode, permutationOutput);
        }
        return sparseQRLeastSquares<double, Eigen::NaturalOrdering<signedIndexType>>(
            S, B, nLhs, mode, permutationOutput);
    case NLS_SINGLE:
        if (nLhs == 3) {
            return sparseQRLeastSquares<single, Eigen::COLAMDOrdering<signedIndexType>>(
                S, B, nLhs, mode, permutationOutput);
        }
        return sparseQRLeastSquares<single, Eigen::NaturalOrdering<signedIndexType>>(
            S, B, nLhs, mode, permutationOutput);
    case NLS_DCOMPLEX:
        if (nLhs == 3) {
            return sparseQRLeastSquares<doublecomplex, Eigen::COLAMDOrdering<signedIndexType>>(
                S, B, nLhs, mode, permutationOutput);
        }
        return sparseQRLeastSquares<doublecomplex, Eigen::NaturalOrdering<signedIndexType>>(
            S, B, nLhs, mode, permutationOutput);
    case NLS_SCOMPLEX:
        if (nLhs == 3) {
            return sparseQRLeastSquares<singlecomplex, Eigen::COLAMDOrdering<signedIndexType>>(
                S, B, nLhs, mode, permutationOutput);
        }
        return sparseQRLeastSquares<singlecomplex, Eigen::NaturalOrdering<signedIndexType>>(
            S, B, nLhs, mode, permutationOutput);
    default:
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
            L"single or double");
    }
    return {};
}
//=============================================================================
} // namespace Nelson
//=============================================================================
