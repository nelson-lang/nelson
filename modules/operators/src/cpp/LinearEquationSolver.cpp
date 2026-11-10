//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cstring>
#include <algorithm>
#include <complex>
#include <cmath>
#include <limits>
#include <vector>
#include "lapack_eigen_config.hpp"
#include "LapackPrototypes.hpp"
#include "LuFactorization.hpp"
#include <Eigen/Dense>
#include "LinearEquationSolver.hpp"
#include "NewWithException.hpp"
#include "i18n.hpp"
#include "Warning.hpp"
#include "ErrorID.hpp"
#include "Error.hpp"
#include "WarningIds.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class R>
static void
setConditionWarning(
    R rcond, R epsilon, bool singular, std::wstring& warningId, std::string& warningMessage)
{
    if (singular || rcond == R(0)) {
        warningId = L"Nelson:singularMatrix";
        warningMessage = resolveMessageTemplate(
            "Nelson:singularMatrix", "Matrix is singular to working precision.");
    } else if (std::isnan(rcond)) {
        warningId = L"Nelson:illConditionedMatrix";
        warningMessage = resolveMessageTemplate("Nelson:operators:matrixIllConditioned",
            "Matrix is singular, nearly singular or badly scaled, results may be inaccurate. "
            "RCOND = NaN.");
    } else if (rcond < epsilon) {
        std::string format = resolveMessageTemplate("Nelson:operators:matrixNearlySingular",
            "Matrix is nearly singular or badly scaled, results may be inaccurate. RCOND = %e.");
        char buffer[1024];
        snprintf(buffer, sizeof(buffer), format.c_str(), static_cast<double>(rcond));
        warningId = WARNING_NEARLY_SINGULAR_MATRIX;
        warningMessage = buffer;
    }
}
//=============================================================================
#if !defined(_MKL_LAPACK_H_)
extern "C"
{
    void LAPACK_GLOBAL(strtrs, STRTRS)(const char*, const char*, const char*, const lapack_int*,
        const lapack_int*, const float*, const lapack_int*, float*, const lapack_int*, lapack_int*);
    void LAPACK_GLOBAL(dtrtrs, DTRTRS)(const char*, const char*, const char*, const lapack_int*,
        const lapack_int*, const double*, const lapack_int*, double*, const lapack_int*,
        lapack_int*);
    void LAPACK_GLOBAL(ctrtrs, CTRTRS)(const char*, const char*, const char*, const lapack_int*,
        const lapack_int*, const std::complex<float>*, const lapack_int*, std::complex<float>*,
        const lapack_int*, lapack_int*);
    void LAPACK_GLOBAL(ztrtrs, ZTRTRS)(const char*, const char*, const char*, const lapack_int*,
        const lapack_int*, const std::complex<double>*, const lapack_int*, std::complex<double>*,
        const lapack_int*, lapack_int*);
}
#endif
//=============================================================================
template <class T> struct DenseSolverTraits;
//=============================================================================
template <> struct DenseSolverTraits<double>
{
    using Real = double;
    static constexpr NelsonType nelsonClass = NLS_DOUBLE;
    static constexpr char conjTrans = 'T';
    static void
    getrf(lapack_int* n, double* a, lapack_int* ipiv, lapack_int* info)
    {
        luFactor(n, n, a, n, ipiv, info);
    }
    static void
    getrs(char trans, lapack_int* n, lapack_int* nrhs, const double* a, const lapack_int* ipiv,
        double* b, lapack_int* info)
    {
        LAPACK_dgetrs(&trans, n, nrhs, a, n, ipiv, b, n, info);
    }
    static void
    gecon(lapack_int* n, const double* a, Real anorm, Real* rcond, lapack_int* info)
    {
        char norm = '1';
        std::vector<double> work((size_t)4 * (size_t)*n);
        std::vector<lapack_int> iwork((size_t)*n);
        LAPACK_dgecon(&norm, n, a, n, &anorm, rcond, work.data(), iwork.data(), info);
    }
    static void
    posv(lapack_int* n, lapack_int* nrhs, double* a, double* b, lapack_int* info)
    {
        char uplo = 'U';
        LAPACK_dposv(&uplo, n, nrhs, a, n, b, n, info);
    }
    static void
    pocon(lapack_int* n, const double* a, Real anorm, Real* rcond, lapack_int* info)
    {
        char uplo = 'U';
        std::vector<double> work((size_t)3 * (size_t)*n);
        std::vector<lapack_int> iwork((size_t)*n);
        LAPACK_dpocon(&uplo, n, a, n, &anorm, rcond, work.data(), iwork.data(), info);
    }
    static void
    trtrs(char uplo, char trans, lapack_int* n, lapack_int* nrhs, const double* a, double* b,
        lapack_int* info)
    {
        char diag = 'N';
        LAPACK_GLOBAL(dtrtrs, DTRTRS)(&uplo, &trans, &diag, n, nrhs, a, n, b, n, info);
    }
};
//=============================================================================
template <> struct DenseSolverTraits<single>
{
    using Real = single;
    static constexpr NelsonType nelsonClass = NLS_SINGLE;
    static constexpr char conjTrans = 'T';
    static void
    getrf(lapack_int* n, single* a, lapack_int* ipiv, lapack_int* info)
    {
        luFactor(n, n, a, n, ipiv, info);
    }
    static void
    getrs(char trans, lapack_int* n, lapack_int* nrhs, const single* a, const lapack_int* ipiv,
        single* b, lapack_int* info)
    {
        LAPACK_sgetrs(&trans, n, nrhs, a, n, ipiv, b, n, info);
    }
    static void
    gecon(lapack_int* n, const single* a, Real anorm, Real* rcond, lapack_int* info)
    {
        char norm = '1';
        std::vector<single> work((size_t)4 * (size_t)*n);
        std::vector<lapack_int> iwork((size_t)*n);
        LAPACK_sgecon(&norm, n, a, n, &anorm, rcond, work.data(), iwork.data(), info);
    }
    static void
    posv(lapack_int* n, lapack_int* nrhs, single* a, single* b, lapack_int* info)
    {
        char uplo = 'U';
        LAPACK_sposv(&uplo, n, nrhs, a, n, b, n, info);
    }
    static void
    pocon(lapack_int* n, const single* a, Real anorm, Real* rcond, lapack_int* info)
    {
        char uplo = 'U';
        std::vector<single> work((size_t)3 * (size_t)*n);
        std::vector<lapack_int> iwork((size_t)*n);
        LAPACK_spocon(&uplo, n, a, n, &anorm, rcond, work.data(), iwork.data(), info);
    }
    static void
    trtrs(char uplo, char trans, lapack_int* n, lapack_int* nrhs, const single* a, single* b,
        lapack_int* info)
    {
        char diag = 'N';
        LAPACK_GLOBAL(strtrs, STRTRS)(&uplo, &trans, &diag, n, nrhs, a, n, b, n, info);
    }
};
//=============================================================================
template <> struct DenseSolverTraits<std::complex<double>>
{
    using Real = double;
    using Z = std::complex<double>;
    static constexpr NelsonType nelsonClass = NLS_DCOMPLEX;
    static constexpr char conjTrans = 'C';
    static void
    getrf(lapack_int* n, Z* a, lapack_int* ipiv, lapack_int* info)
    {
        luFactor(n, n, a, n, ipiv, info);
    }
    static void
    getrs(char trans, lapack_int* n, lapack_int* nrhs, const Z* a, const lapack_int* ipiv, Z* b,
        lapack_int* info)
    {
        LAPACK_zgetrs(&trans, n, nrhs, a, n, ipiv, b, n, info);
    }
    static void
    gecon(lapack_int* n, const Z* a, Real anorm, Real* rcond, lapack_int* info)
    {
        char norm = '1';
        std::vector<Z> work((size_t)2 * (size_t)*n);
        std::vector<double> rwork((size_t)2 * (size_t)*n);
        LAPACK_zgecon(&norm, n, a, n, &anorm, rcond, work.data(), rwork.data(), info);
    }
    static void
    posv(lapack_int* n, lapack_int* nrhs, Z* a, Z* b, lapack_int* info)
    {
        char uplo = 'U';
        LAPACK_zposv(&uplo, n, nrhs, a, n, b, n, info);
    }
    static void
    pocon(lapack_int* n, const Z* a, Real anorm, Real* rcond, lapack_int* info)
    {
        char uplo = 'U';
        std::vector<Z> work((size_t)2 * (size_t)*n);
        std::vector<double> rwork((size_t)*n);
        LAPACK_zpocon(&uplo, n, a, n, &anorm, rcond, work.data(), rwork.data(), info);
    }
    static void
    trtrs(
        char uplo, char trans, lapack_int* n, lapack_int* nrhs, const Z* a, Z* b, lapack_int* info)
    {
        char diag = 'N';
        LAPACK_GLOBAL(ztrtrs, ZTRTRS)(&uplo, &trans, &diag, n, nrhs, a, n, b, n, info);
    }
};
//=============================================================================
template <> struct DenseSolverTraits<std::complex<single>>
{
    using Real = single;
    using Z = std::complex<single>;
    static constexpr NelsonType nelsonClass = NLS_SCOMPLEX;
    static constexpr char conjTrans = 'C';
    static void
    getrf(lapack_int* n, Z* a, lapack_int* ipiv, lapack_int* info)
    {
        luFactor(n, n, a, n, ipiv, info);
    }
    static void
    getrs(char trans, lapack_int* n, lapack_int* nrhs, const Z* a, const lapack_int* ipiv, Z* b,
        lapack_int* info)
    {
        LAPACK_cgetrs(&trans, n, nrhs, a, n, ipiv, b, n, info);
    }
    static void
    gecon(lapack_int* n, const Z* a, Real anorm, Real* rcond, lapack_int* info)
    {
        char norm = '1';
        std::vector<Z> work((size_t)2 * (size_t)*n);
        std::vector<single> rwork((size_t)2 * (size_t)*n);
        LAPACK_cgecon(&norm, n, a, n, &anorm, rcond, work.data(), rwork.data(), info);
    }
    static void
    posv(lapack_int* n, lapack_int* nrhs, Z* a, Z* b, lapack_int* info)
    {
        char uplo = 'U';
        LAPACK_cposv(&uplo, n, nrhs, a, n, b, n, info);
    }
    static void
    pocon(lapack_int* n, const Z* a, Real anorm, Real* rcond, lapack_int* info)
    {
        char uplo = 'U';
        std::vector<Z> work((size_t)2 * (size_t)*n);
        std::vector<single> rwork((size_t)*n);
        LAPACK_cpocon(&uplo, n, a, n, &anorm, rcond, work.data(), rwork.data(), info);
    }
    static void
    trtrs(
        char uplo, char trans, lapack_int* n, lapack_int* nrhs, const Z* a, Z* b, lapack_int* info)
    {
        char diag = 'N';
        LAPACK_GLOBAL(ctrtrs, CTRTRS)(&uplo, &trans, &diag, n, nrhs, a, n, b, n, info);
    }
};
//=============================================================================
template <class T>
static bool
isLowerTriangularMatrix(const T* a, indexType n)
{
    for (indexType col = 1; col < n; ++col) {
        for (indexType row = 0; row < col; ++row) {
            if (a[row + col * n] != T(0)) {
                return false;
            }
        }
    }
    return true;
}
//=============================================================================
template <class T>
static bool
isUpperTriangularMatrix(const T* a, indexType n)
{
    for (indexType col = 0; col + 1 < n; ++col) {
        for (indexType row = col + 1; row < n; ++row) {
            if (a[row + col * n] != T(0)) {
                return false;
            }
        }
    }
    return true;
}
//=============================================================================
// Substitution with triangular A, or with A' when adjoint is set.
template <class T>
static void
substituteTriangular(const T* a, indexType n, bool upper, bool adjoint, T* x, indexType nrhs)
{
    auto at = [&](indexType row, indexType col) {
        return adjoint ? luConj(a[col + row * n]) : a[row + col * n];
    };
    bool backward = upper != adjoint;
    for (indexType col = 0; col < nrhs; ++col) {
        T* b = x + col * n;
        if (backward) {
            for (indexType k = n; k-- > 0;) {
                b[k] = luDivide(b[k], at(k, k));
                for (indexType i = 0; i < k; ++i) {
                    b[i] -= b[k] * at(i, k);
                }
            }
        } else {
            for (indexType k = 0; k < n; ++k) {
                b[k] = luDivide(b[k], at(k, k));
                for (indexType i = k + 1; i < n; ++i) {
                    b[i] -= b[k] * at(i, k);
                }
            }
        }
    }
}
//=============================================================================
// Solves with an LU factor that has a zero pivot, where getrs would stop.
template <class T>
static void
substituteSingularLu(
    const T* lu, const lapack_int* ipiv, indexType n, bool transposed, T* x, indexType nrhs)
{
    for (indexType col = 0; col < nrhs; ++col) {
        T* b = x + col * n;
        if (!transposed) {
            for (indexType i = 0; i < n; ++i) {
                indexType p = (indexType)ipiv[i] - 1;
                if (p != i) {
                    std::swap(b[i], b[p]);
                }
            }
            for (indexType k = 0; k < n; ++k) {
                for (indexType i = k + 1; i < n; ++i) {
                    b[i] -= b[k] * lu[i + k * n];
                }
            }
            for (indexType k = n; k-- > 0;) {
                b[k] = luDivide(b[k], lu[k + k * n]);
                for (indexType i = 0; i < k; ++i) {
                    b[i] -= b[k] * lu[i + k * n];
                }
            }
        } else {
            for (indexType k = 0; k < n; ++k) {
                b[k] = luDivide(b[k], luConj(lu[k + k * n]));
                for (indexType i = k + 1; i < n; ++i) {
                    b[i] -= b[k] * luConj(lu[k + i * n]);
                }
            }
            for (indexType k = n; k-- > 0;) {
                for (indexType i = 0; i < k; ++i) {
                    b[i] -= b[k] * luConj(lu[k + i * n]);
                }
            }
            for (indexType i = n; i-- > 0;) {
                indexType p = (indexType)ipiv[i] - 1;
                if (p != i) {
                    std::swap(b[i], b[p]);
                }
            }
        }
    }
}
//=============================================================================
template <class T>
static void
conjugateInPlace(T*, size_t)
{
}
//=============================================================================
template <class T>
static void
conjugateInPlace(std::complex<T>* x, size_t count)
{
    for (size_t k = 0; k < count; ++k) {
        x[k] = std::conj(x[k]);
    }
}
//=============================================================================
// How the square divisor D is stored in A and which system is solved.
enum class SolveForm
{
    Left, // A = D, solves D * x = b
    AdjointStored, // A = D', solves D' * x = b and factors D
    Right // A = D, solves D' * x = b' for b / D
};
//=============================================================================
template <class T> struct SquareSystem
{
    const T* A;
    indexType n;
    const T* B;
    indexType nrhs;
    SolveForm form;
};
//=============================================================================
// Right-hand side buffer: a copy of B, or B' for a right division.
template <class T>
static T*
makeRightHandSide(const SquareSystem<T>& sys)
{
    size_t count = (size_t)sys.n * (size_t)sys.nrhs;
    T* x = new_with_exception<T>(count, false);
    if (sys.form == SolveForm::Right && sys.nrhs != 1) {
        using Matrix = Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>;
        Eigen::Map<const Matrix> b(sys.B, (Eigen::Index)sys.nrhs, (Eigen::Index)sys.n);
        Eigen::Map<Matrix>(x, (Eigen::Index)sys.n, (Eigen::Index)sys.nrhs) = b.adjoint();
    } else {
        memcpy(x, sys.B, count * sizeof(T));
        if (sys.form == SolveForm::Right) {
            conjugateInPlace(x, count);
        }
    }
    return x;
}
//=============================================================================
// 1-norm of A, or of its conjugate transpose (largest row sum); NaN wins like ?lange.
template <class T>
static typename DenseSolverTraits<T>::Real
matrixOneNorm(const T* a, indexType n, bool transposed)
{
    using Real = typename DenseSolverTraits<T>::Real;
    using Matrix = Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>;
    Eigen::Map<const Matrix> A(a, (Eigen::Index)n, (Eigen::Index)n);
    Eigen::Matrix<Real, Eigen::Dynamic, 1> sums = transposed
        ? Eigen::Matrix<Real, Eigen::Dynamic, 1>(A.cwiseAbs().rowwise().sum())
        : Eigen::Matrix<Real, Eigen::Dynamic, 1>(A.cwiseAbs().colwise().sum().transpose());
    Real norm = 0;
    for (Eigen::Index k = 0; k < sums.size(); ++k) {
        if (std::isnan(sums[k]) || sums[k] > norm) {
            norm = sums[k];
        }
    }
    return norm;
}
//=============================================================================
// In-place solves with an LU or Cholesky factor of order n stored by columns.
template <class T> using DenseVector = Eigen::Matrix<T, Eigen::Dynamic, 1>;
template <class T> using VectorView = Eigen::Map<DenseVector<T>>;
template <class T> using ConstVectorView = Eigen::Map<const DenseVector<T>>;
//=============================================================================
// Short segments use plain loops, longer ones the vectorized Eigen kernels.
constexpr indexType shortSegment = 32;
//=============================================================================
// x[first:last) -= xj * column[first:last)
template <class T>
static inline void
subtractScaledColumn(T* x, const T* column, indexType first, indexType last, const T& xj)
{
    if (last <= first || xj == T(0)) {
        return;
    }
    if (last - first < shortSegment) {
        for (indexType i = first; i < last; ++i) {
            x[i] -= xj * column[i];
        }
        return;
    }
    VectorView<T>(x + first, (Eigen::Index)(last - first)).noalias()
        -= xj * ConstVectorView<T>(column + first, (Eigen::Index)(last - first));
}
//=============================================================================
// sum of conj(column[i]) * x[i] over [first:last)
template <class T>
static inline T
adjointDot(const T* column, const T* x, indexType first, indexType last)
{
    if (last <= first || last - first < shortSegment) {
        T sum = T(0);
        for (indexType i = first; i < last; ++i) {
            sum += luConj(column[i]) * x[i];
        }
        return sum;
    }
    return ConstVectorView<T>(column + first, (Eigen::Index)(last - first))
        .dot(ConstVectorView<T>(x + first, (Eigen::Index)(last - first)));
}
//=============================================================================
template <class T>
static void
solveUnitLower(const T* factor, indexType n, T* x)
{
    for (indexType j = 0; j < n; ++j) {
        subtractScaledColumn<T>(x, factor + j * n, j + 1, n, x[j]);
    }
}
//=============================================================================
template <class T>
static void
solveUpper(const T* factor, indexType n, T* x)
{
    for (indexType j = n; j-- > 0;) {
        x[j] /= factor[j + j * n];
        subtractScaledColumn<T>(x, factor + j * n, 0, j, x[j]);
    }
}
//=============================================================================
template <class T>
static void
solveUpperAdjoint(const T* factor, indexType n, T* x)
{
    for (indexType j = 0; j < n; ++j) {
        const T* column = factor + j * n;
        x[j] = (x[j] - adjointDot<T>(column, x, 0, j)) / luConj(column[j]);
    }
}
//=============================================================================
template <class T>
static void
solveUnitLowerAdjoint(const T* factor, indexType n, T* x)
{
    for (indexType j = n; j-- > 0;) {
        x[j] -= adjointDot<T>(factor + j * n, x, j + 1, n);
    }
}
//=============================================================================
// Sign step of the estimator: +1/-1 for real values, unit phase for complex ones.
template <class T>
static void
replaceBySigns(T* x, indexType n)
{
    for (indexType i = 0; i < n; ++i) {
        x[i] = x[i] >= T(0) ? T(1) : T(-1);
    }
}
//=============================================================================
template <class T>
static void
replaceBySigns(std::complex<T>* x, indexType n)
{
    for (indexType i = 0; i < n; ++i) {
        T modulus = std::abs(x[i]);
        x[i] = modulus > std::numeric_limits<T>::min()
            ? std::complex<T>(x[i].real() / modulus, x[i].imag() / modulus)
            : std::complex<T>(1);
    }
}
//=============================================================================
// Real estimates stop when the sign vector repeats; complex ones never do.
template <class T>
static bool
signsRepeat(const T* x, const T* signs, indexType n)
{
    for (indexType i = 0; i < n; ++i) {
        if ((x[i] >= T(0) ? T(1) : T(-1)) != signs[i]) {
            return false;
        }
    }
    return true;
}
//=============================================================================
template <class T>
static bool
signsRepeat(const std::complex<T>*, const std::complex<T>*, indexType)
{
    return false;
}
//=============================================================================
template <class T>
static bool
differsFromMaximum(const T& previous, const T& maximum)
{
    return previous != std::abs(maximum);
}
//=============================================================================
template <class T>
static bool
differsFromMaximum(const std::complex<T>& previous, const std::complex<T>& maximum)
{
    return std::abs(previous) != std::abs(maximum);
}
//=============================================================================
template <class T>
static typename DenseSolverTraits<T>::Real
sumOfModuli(const std::vector<T>& x)
{
    typename DenseSolverTraits<T>::Real sum = 0;
    for (const T& value : x) {
        sum += std::abs(value);
    }
    return sum;
}
//=============================================================================
template <class T>
static indexType
indexOfLargestModulus(const std::vector<T>& x)
{
    indexType index = 0;
    typename DenseSolverTraits<T>::Real largest = std::abs(x[0]);
    for (indexType i = 1; i < (indexType)x.size(); ++i) {
        if (std::abs(x[i]) > largest) {
            largest = std::abs(x[i]);
            index = i;
        }
    }
    return index;
}
//=============================================================================
// 1-norm estimate of inv(A) with the ?lacn2 steps; apply(x, adjoint) replaces x by inv(A) x
// or by inv(A)' x.
template <class T, class Apply>
static typename DenseSolverTraits<T>::Real
estimateInverseOneNorm(indexType n, Apply apply)
{
    using Real = typename DenseSolverTraits<T>::Real;
    constexpr int maxIterations = 5;
    std::vector<T> x((size_t)n, T(Real(1) / Real(n)));
    std::vector<T> signs((size_t)n);
    apply(x.data(), false);
    if (n == 1) {
        return std::abs(x[0]);
    }
    Real estimate = sumOfModuli(x);
    replaceBySigns(x.data(), n);
    signs = x;
    apply(x.data(), true);
    indexType j = indexOfLargestModulus(x);
    int iteration = 2;
    while (true) {
        std::fill(x.begin(), x.end(), T(0));
        x[j] = T(1);
        apply(x.data(), false);
        Real previous = estimate;
        estimate = sumOfModuli(x);
        if (signsRepeat(x.data(), signs.data(), n) || estimate <= previous) {
            break;
        }
        replaceBySigns(x.data(), n);
        signs = x;
        apply(x.data(), true);
        indexType last = j;
        j = indexOfLargestModulus(x);
        if (!differsFromMaximum(x[last], x[j]) || iteration >= maxIterations) {
            break;
        }
        ++iteration;
    }
    Real alternate = 1;
    for (indexType i = 0; i < n; ++i) {
        x[i] = T(alternate * (Real(1) + Real(i) / Real(n - 1)));
        alternate = -alternate;
    }
    apply(x.data(), false);
    Real extra = Real(2) * (sumOfModuli(x) / Real(3 * n));
    return extra > estimate ? extra : estimate;
}
//=============================================================================
// RCOND of ?gecon / ?pocon; LAPACK recomputes it only near eps or after an overflow.
template <class T>
static typename DenseSolverTraits<T>::Real
reciprocalCondition(
    const T* factor, indexType n, typename DenseSolverTraits<T>::Real anorm, bool cholesky)
{
    using Traits = DenseSolverTraits<T>;
    using Real = typename Traits::Real;
    if (anorm == Real(0)) {
        return Real(0);
    }
    auto apply = [&](T* x, bool adjoint) {
        if (cholesky) {
            solveUpperAdjoint<T>(factor, n, x);
            solveUpper<T>(factor, n, x);
        } else if (adjoint) {
            solveUpperAdjoint<T>(factor, n, x);
            solveUnitLowerAdjoint<T>(factor, n, x);
        } else {
            solveUnitLower<T>(factor, n, x);
            solveUpper<T>(factor, n, x);
        }
    };
    Real inverseNorm = estimateInverseOneNorm<T>(n, apply);
    Real rcond = inverseNorm != Real(0) ? (Real(1) / inverseNorm) / anorm : Real(0);
    if (rcond >= Real(1024) * std::numeric_limits<Real>::epsilon()) {
        return rcond;
    }
    lapack_int N = (lapack_int)n;
    lapack_int info = 0;
    rcond = 0;
    if (cholesky) {
        Traits::pocon(&N, factor, anorm, &rcond, &info);
    } else {
        Traits::gecon(&N, factor, anorm, &rcond, &info);
    }
    return rcond;
}
//=============================================================================
template <class T>
static bool
isHermitianMatrix(const T* data, indexType n)
{
    for (indexType col = 0; col < n; ++col) {
        if (std::imag(data[col + col * n]) != 0) {
            return false;
        }
        for (indexType row = 0; row < col; ++row) {
            if (data[row + col * n] != luConj(data[col + row * n])) {
                return false;
            }
        }
    }
    return true;
}
//=============================================================================
// Hermitian A: Cholesky, left to LU when it fails or its RCOND is below eps.
template <class T>
static T*
solvePositiveDefinite(const SquareSystem<T>& sys)
{
    using Traits = DenseSolverTraits<T>;
    using Real = typename Traits::Real;
    indexType m = sys.n;
    const T* A = sys.A;
    if (!isHermitianMatrix<T>(A, m)) {
        return nullptr;
    }
    lapack_int N = (lapack_int)m;
    Real anorm = matrixOneNorm<T>(A, m, false);
    if (!std::isfinite(anorm)) {
        return nullptr;
    }
    lapack_int NRHS = (lapack_int)sys.nrhs;
    std::vector<T> factor(A, A + (size_t)m * (size_t)m);
    T* x = makeRightHandSide<T>(sys);
    lapack_int info = 0;
    Real rcond = 0;
    Traits::posv(&N, &NRHS, factor.data(), x, &info);
    if (info == 0) {
        rcond = reciprocalCondition<T>(factor.data(), m, anorm, true);
    }
    if (info != 0 || !(rcond >= std::numeric_limits<Real>::epsilon())) {
        delete[] x;
        return nullptr;
    }
    return x;
}
//=============================================================================
// Triangular D: lower is tried first, so a stored adjoint is tried upper first.
template <class T>
static T*
solveTriangular(const SquareSystem<T>& sys, std::wstring& warningId, std::string& warningMessage)
{
    using Traits = DenseSolverTraits<T>;
    using Real = typename Traits::Real;
    indexType m = sys.n;
    const T* A = sys.A;
    bool upper = false;
    if (sys.form == SolveForm::AdjointStored) {
        if (isUpperTriangularMatrix<T>(A, m)) {
            upper = true;
        } else if (!isLowerTriangularMatrix<T>(A, m)) {
            return nullptr;
        }
    } else {
        if (!isLowerTriangularMatrix<T>(A, m)) {
            if (!isUpperTriangularMatrix<T>(A, m)) {
                return nullptr;
            }
            upper = true;
        }
    }
    bool adjoint = sys.form == SolveForm::Right;
    indexType nrhs = sys.nrhs;
    T* x = makeRightHandSide<T>(sys);
    bool zeroDiagonal = false;
    for (indexType k = 0; k < m; ++k) {
        if (A[k + k * m] == T(0)) {
            zeroDiagonal = true;
            break;
        }
    }
    lapack_int N = (lapack_int)m;
    lapack_int NRHS = (lapack_int)nrhs;
    lapack_int info = 0;
    Real rcond = 0;
    if (zeroDiagonal) {
        substituteTriangular<T>(A, m, upper, adjoint, x, nrhs);
    } else {
        // Triangular RCOND is the diagonal ratio min|d| / max|d|.
        Real minDiagonal = std::numeric_limits<Real>::infinity();
        Real maxDiagonal = 0;
        for (indexType k = 0; k < m; ++k) {
            Real value = static_cast<Real>(std::abs(A[k + k * m]));
            if (std::isnan(value)) {
                minDiagonal = value;
                break;
            }
            minDiagonal = std::min(minDiagonal, value);
            maxDiagonal = std::max(maxDiagonal, value);
        }
        rcond = minDiagonal / maxDiagonal;
        // small systems keep the substitution order of the small LU solves
        if (isSmallLu<T>(N, N)) {
            if (adjoint) {
                smallTriangularSolveRows<T>(A, N, upper, x, NRHS);
            } else {
                smallTriangularSolve<T>(A, N, upper, x, NRHS);
            }
        } else {
            Traits::trtrs(
                upper ? 'U' : 'L', adjoint ? Traits::conjTrans : 'N', &N, &NRHS, A, x, &info);
            if (info != 0) {
                substituteTriangular<T>(A, m, upper, adjoint, x, nrhs);
            }
        }
    }
    setConditionWarning<Real>(
        rcond, std::numeric_limits<Real>::epsilon(), zeroDiagonal, warningId, warningMessage);
    return x;
}
//=============================================================================
// General square D: LU with partial pivoting of D itself, like the reference solver.
template <class T>
static T*
solveGeneralLu(const SquareSystem<T>& sys, std::wstring& warningId, std::string& warningMessage)
{
    using Traits = DenseSolverTraits<T>;
    using Real = typename Traits::Real;
    indexType m = sys.n;
    indexType nrhs = sys.nrhs;
    const T* A = sys.A;
    lapack_int N = (lapack_int)m;
    lapack_int NRHS = (lapack_int)nrhs;
    bool storedAdjoint = sys.form == SolveForm::AdjointStored;
    bool adjoint = sys.form != SolveForm::Left;

    std::vector<T> factor((size_t)m * (size_t)m);
    if (storedAdjoint) {
        for (indexType col = 0; col < m; ++col) {
            for (indexType row = 0; row < m; ++row) {
                factor[col + row * m] = luConj(A[row + col * m]);
            }
        }
    } else {
        memcpy(factor.data(), A, (size_t)m * (size_t)m * sizeof(T));
    }
    T* x = makeRightHandSide<T>(sys);
    std::vector<lapack_int> ipiv((size_t)std::max<lapack_int>(1, N));
    lapack_int info = 0;
    Traits::getrf(&N, factor.data(), ipiv.data(), &info);
    if (info < 0) {
        delete[] x;
        Error(L"Nelson:operators:solverFailed", L"Linear equation solver failed.");
    }
    Real rcond = 1;
    bool zeroPivot = info > 0;
    if (zeroPivot) {
        rcond = 0;
    } else {
        // RCOND is always estimated, so balanced pivots do not hide a bad condition.
        Real anorm = matrixOneNorm<T>(A, m, storedAdjoint);
        if (std::isnan(anorm)) {
            rcond = std::numeric_limits<Real>::quiet_NaN();
        } else if (std::isinf(anorm)) {
            rcond = 0;
        } else {
            rcond = reciprocalCondition<T>(factor.data(), m, anorm, false);
        }
    }
    // small systems keep the substitution order of the unblocked factor
    if (isSmallLu<T>(N, N)) {
        if (adjoint) {
            smallLuSolveRows<T>(factor.data(), N, ipiv.data(), x, NRHS);
        } else {
            smallLuSolve<T>(factor.data(), N, ipiv.data(), x, NRHS);
        }
    } else if (zeroPivot) {
        substituteSingularLu<T>(factor.data(), ipiv.data(), m, adjoint, x, nrhs);
    } else {
        char trans = adjoint ? Traits::conjTrans : 'N';
        Traits::getrs(trans, &N, &NRHS, factor.data(), ipiv.data(), x, &info);
    }
    setConditionWarning<Real>(
        rcond, std::numeric_limits<Real>::epsilon(), zeroPivot, warningId, warningMessage);
    return x;
}
//=============================================================================
// Returns the n x nrhs solution buffer.
template <class T>
static T*
solveSquareSystem(const SquareSystem<T>& sys, std::wstring& warningId, std::string& warningMessage)
{
    warningId.clear();
    warningMessage.clear();
    T* x = solveTriangular<T>(sys, warningId, warningMessage);
    if (x == nullptr) {
        x = solvePositiveDefinite<T>(sys);
    }
    if (x == nullptr) {
        x = solveGeneralLu<T>(sys, warningId, warningMessage);
    }
    return x;
}
//=============================================================================
template <class T>
static ArrayOf
solveLinearEquation(const ArrayOf& matA, const ArrayOf& matB, bool transposed,
    std::wstring& warningId, std::string& warningMessage)
{
    SquareSystem<T> sys { reinterpret_cast<const T*>(matA.dataPointer()), matA.dimensionLength(0),
        reinterpret_cast<const T*>(matB.dataPointer()), matB.dimensionLength(1),
        transposed ? SolveForm::AdjointStored : SolveForm::Left };
    T* x = solveSquareSystem<T>(sys, warningId, warningMessage);
    return ArrayOf(DenseSolverTraits<T>::nelsonClass, Dimensions(sys.n, sys.nrhs), (void*)x);
}
//=============================================================================
// B / D solved as D' * X' = B' with D factored as given, then X' is turned back.
template <class T>
static bool
solveRightDivision(const ArrayOf& numerator, const ArrayOf& divisor, ArrayOf& result,
    std::wstring& warningId, std::string& warningMessage)
{
    indexType n = divisor.dimensionLength(0);
    const T* D = reinterpret_cast<const T*>(divisor.dataPointer());
    indexType rows = numerator.dimensionLength(0);
    SquareSystem<T> sys { D, n, reinterpret_cast<const T*>(numerator.dataPointer()), rows,
        SolveForm::Right };
    T* x = solveSquareSystem<T>(sys, warningId, warningMessage);
    if (rows == 1) {
        conjugateInPlace(x, (size_t)n);
    } else {
        using Matrix = Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>;
        T* r = nullptr;
        try {
            r = new_with_exception<T>((size_t)rows * (size_t)n, false);
        } catch (...) {
            delete[] x;
            throw;
        }
        Eigen::Map<Matrix>(r, (Eigen::Index)rows, (Eigen::Index)n)
            = Eigen::Map<const Matrix>(x, (Eigen::Index)n, (Eigen::Index)rows).adjoint();
        delete[] x;
        x = r;
    }
    result = ArrayOf(DenseSolverTraits<T>::nelsonClass, Dimensions(rows, n), (void*)x);
    return true;
}
//=============================================================================
bool
solveRightLinearEquation(const ArrayOf& numerator, const ArrayOf& divisor, ArrayOf& result,
    std::wstring& warningId, std::string& warningMessage)
{
    switch (divisor.dataClass()) {
    case NLS_DOUBLE:
        return solveRightDivision<double>(numerator, divisor, result, warningId, warningMessage);
    case NLS_SINGLE:
        return solveRightDivision<single>(numerator, divisor, result, warningId, warningMessage);
    case NLS_DCOMPLEX:
        return solveRightDivision<std::complex<double>>(
            numerator, divisor, result, warningId, warningMessage);
    case NLS_SCOMPLEX:
        return solveRightDivision<std::complex<single>>(
            numerator, divisor, result, warningId, warningMessage);
    default:
        return false;
    }
}
//=============================================================================
ArrayOf
solveLinearEquationDouble(const ArrayOf& matA, const ArrayOf& matB, std::wstring& warningId,
    std::string& warningMessage, bool transposed)
{
    return solveLinearEquation<double>(matA, matB, transposed, warningId, warningMessage);
}
//=============================================================================
ArrayOf
solveLinearEquationDoubleComplex(const ArrayOf& matA, const ArrayOf& matB, std::wstring& warningId,
    std::string& warningMessage, bool transposed)
{
    return solveLinearEquation<std::complex<double>>(
        matA, matB, transposed, warningId, warningMessage);
}
//=============================================================================
ArrayOf
solveLinearEquationSingle(const ArrayOf& matA, const ArrayOf& matB, std::wstring& warningId,
    std::string& warningMessage, bool transposed)
{
    return solveLinearEquation<single>(matA, matB, transposed, warningId, warningMessage);
}
//=============================================================================
ArrayOf
solveLinearEquationSingleComplex(const ArrayOf& matA, const ArrayOf& matB, std::wstring& warningId,
    std::string& warningMessage, bool transposed)
{
    return solveLinearEquation<std::complex<single>>(
        matA, matB, transposed, warningId, warningMessage);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
