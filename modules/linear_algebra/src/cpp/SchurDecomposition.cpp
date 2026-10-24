//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "SchurDecomposition.hpp"
#include "ClassName.hpp"
#include "LapackPrototypes.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include <algorithm>
#include <cmath>
#include <complex>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
template <typename T> struct SchurLapackTraits;
//=============================================================================
template <> struct SchurLapackTraits<single>
{
    static constexpr NelsonType nelsonClass = NLS_SINGLE;
    static void
    gees(char* jobvs, char* sort, lapack_int* n, single* a, lapack_int* lda, lapack_int* sdim,
        single* wr, single* wi, single* vs, lapack_int* ldvs, single* work, lapack_int* lwork,
        lapack_int* bwork, lapack_int* info)
    {
        LAPACK_sgees(
            jobvs, sort, nullptr, n, a, lda, sdim, wr, wi, vs, ldvs, work, lwork, bwork, info);
    }
};
//=============================================================================
template <> struct SchurLapackTraits<double>
{
    static constexpr NelsonType nelsonClass = NLS_DOUBLE;
    static void
    gees(char* jobvs, char* sort, lapack_int* n, double* a, lapack_int* lda, lapack_int* sdim,
        double* wr, double* wi, double* vs, lapack_int* ldvs, double* work, lapack_int* lwork,
        lapack_int* bwork, lapack_int* info)
    {
        LAPACK_dgees(
            jobvs, sort, nullptr, n, a, lda, sdim, wr, wi, vs, ldvs, work, lwork, bwork, info);
    }
};
//=============================================================================
template <> struct SchurLapackTraits<singlecomplex>
{
    using Real = single;
    static constexpr NelsonType nelsonClass = NLS_SCOMPLEX;
    static void
    gees(char* jobvs, char* sort, lapack_int* n, singlecomplex* a, lapack_int* lda,
        lapack_int* sdim, singlecomplex* w, singlecomplex* vs, lapack_int* ldvs,
        singlecomplex* work, lapack_int* lwork, Real* rwork, lapack_int* bwork, lapack_int* info)
    {
        LAPACK_cgees(
            jobvs, sort, nullptr, n, a, lda, sdim, w, vs, ldvs, work, lwork, rwork, bwork, info);
    }
};
//=============================================================================
template <> struct SchurLapackTraits<doublecomplex>
{
    using Real = double;
    static constexpr NelsonType nelsonClass = NLS_DCOMPLEX;
    static void
    gees(char* jobvs, char* sort, lapack_int* n, doublecomplex* a, lapack_int* lda,
        lapack_int* sdim, doublecomplex* w, doublecomplex* vs, lapack_int* ldvs,
        doublecomplex* work, lapack_int* lwork, Real* rwork, lapack_int* bwork, lapack_int* info)
    {
        LAPACK_zgees(
            jobvs, sort, nullptr, n, a, lda, sdim, w, vs, ldvs, work, lwork, rwork, bwork, info);
    }
};
//=============================================================================
template <typename T>
static bool
isFiniteValue(const T& value)
{
    return std::isfinite(static_cast<double>(value));
}
//=============================================================================
template <typename T>
static bool
isFiniteValue(const std::complex<T>& value)
{
    return std::isfinite(static_cast<double>(value.real()))
        && std::isfinite(static_cast<double>(value.imag()));
}
//=============================================================================
template <typename T>
static void
checkFiniteInput(const ArrayOf& A)
{
    const T* data = reinterpret_cast<const T*>(A.dataPointer());
    for (indexType k = 0; k < A.elementCount(); ++k) {
        if (!isFiniteValue(data[k])) {
            Error("Nelson:linear_algebra:inputMustBeFinite", "%s must not contain NaN or Inf.",
                "Input argument");
        }
    }
}
//=============================================================================
static void
checkSchurInput(const ArrayOf& A)
{
    bool isSupportedTypes = (A.dataClass() == NLS_DOUBLE || A.dataClass() == NLS_SINGLE
                                || A.dataClass() == NLS_DCOMPLEX || A.dataClass() == NLS_SCOMPLEX)
        && !A.isSparse();
    if (!isSupportedTypes) {
        Error("Nelson:UndefinedFunction:undefinedFunctionValueInputArgumentsType",
            "Undefined function '%s' for input arguments of type '%s'.", "schur", ClassName(A));
    }
    if (!A.isSquare()) {
        Error("Nelson:linear_algebra:squareMatrixExpected", "%s expected.", "Square matrix");
    }
}
//=============================================================================
template <typename T>
static void
computeRealSchur(ArrayOf& schurT, ArrayOf* U)
{
    using Traits = SchurLapackTraits<T>;
    lapack_int n = static_cast<lapack_int>(schurT.rows());
    lapack_int lda = std::max<lapack_int>(1, n);
    lapack_int ldvs = U ? lda : 1;
    std::vector<T> wr(static_cast<size_t>(std::max<lapack_int>(1, n)));
    std::vector<T> wi(static_cast<size_t>(std::max<lapack_int>(1, n)));
    std::vector<T> vs(static_cast<size_t>(ldvs) * static_cast<size_t>(U ? n : 1));
    std::vector<lapack_int> bwork(static_cast<size_t>(std::max<lapack_int>(1, n)));
    lapack_int sdim = 0;
    lapack_int info = 0;
    lapack_int lwork = -1;
    T workQuery {};
    char jobvs = U ? 'V' : 'N';
    char sort = 'N';
    auto* t = reinterpret_cast<T*>(const_cast<void*>(schurT.dataPointer()));
    Traits::gees(&jobvs, &sort, &n, t, &lda, &sdim, wr.data(), wi.data(), vs.data(), &ldvs,
        &workQuery, &lwork, bwork.data(), &info);
    if (info != 0) {
        Error("Nelson:schur:decompositionFailed", "Schur decomposition failed.");
    }
    lwork = std::max<lapack_int>(1, static_cast<lapack_int>(workQuery));
    std::vector<T> work(static_cast<size_t>(lwork));
    Traits::gees(&jobvs, &sort, &n, t, &lda, &sdim, wr.data(), wi.data(), vs.data(), &ldvs,
        work.data(), &lwork, bwork.data(), &info);
    if (info != 0) {
        Error("Nelson:schur:decompositionFailed", "Schur decomposition failed.");
    }
    if (U) {
        T* u = reinterpret_cast<T*>(const_cast<void*>(U->dataPointer()));
        std::copy(vs.begin(), vs.end(), u);
    }
}
//=============================================================================
template <typename T>
static void
computeComplexSchur(ArrayOf& schurT, ArrayOf* U)
{
    using Traits = SchurLapackTraits<T>;
    using Real = typename Traits::Real;
    lapack_int n = static_cast<lapack_int>(schurT.rows());
    lapack_int lda = std::max<lapack_int>(1, n);
    lapack_int ldvs = U ? lda : 1;
    std::vector<T> w(static_cast<size_t>(std::max<lapack_int>(1, n)));
    std::vector<T> vs(static_cast<size_t>(ldvs) * static_cast<size_t>(U ? n : 1));
    std::vector<Real> rwork(static_cast<size_t>(std::max<lapack_int>(1, n)));
    std::vector<lapack_int> bwork(static_cast<size_t>(std::max<lapack_int>(1, n)));
    lapack_int sdim = 0;
    lapack_int info = 0;
    lapack_int lwork = -1;
    T workQuery {};
    char jobvs = U ? 'V' : 'N';
    char sort = 'N';
    auto* t = reinterpret_cast<T*>(const_cast<void*>(schurT.dataPointer()));
    Traits::gees(&jobvs, &sort, &n, t, &lda, &sdim, w.data(), vs.data(), &ldvs, &workQuery, &lwork,
        rwork.data(), bwork.data(), &info);
    if (info != 0) {
        Error("Nelson:schur:decompositionFailed", "Schur decomposition failed.");
    }
    lwork = std::max<lapack_int>(1, static_cast<lapack_int>(std::real(workQuery)));
    std::vector<T> work(static_cast<size_t>(lwork));
    Traits::gees(&jobvs, &sort, &n, t, &lda, &sdim, w.data(), vs.data(), &ldvs, work.data(), &lwork,
        rwork.data(), bwork.data(), &info);
    if (info != 0) {
        Error("Nelson:schur:decompositionFailed", "Schur decomposition failed.");
    }
    if (U) {
        T* u = reinterpret_cast<T*>(const_cast<void*>(U->dataPointer()));
        std::copy(vs.begin(), vs.end(), u);
    }
}
//=============================================================================
static void
promoteRealSchurResult(ArrayOf& value)
{
    if (!value.allReal()) {
        return;
    }
    if (value.dataClass() == NLS_SINGLE || value.dataClass() == NLS_SCOMPLEX) {
        value.promoteType(NLS_SINGLE);
    } else {
        value.promoteType(NLS_DOUBLE);
    }
}
//=============================================================================
void
SchurDecomposition(ArrayOf A, bool asComplex, ArrayOf& T)
{
    checkSchurInput(A);
    Dimensions dimsA = A.dimensions();
    if (A.isEmpty()) {
        T = ArrayOf::empty(dimsA);
        T.promoteType(A.dataClass());
        return;
    }
    if (asComplex || A.isComplex()) {
        if (A.dataClass() == NLS_SINGLE || A.dataClass() == NLS_SCOMPLEX) {
            A.promoteType(NLS_SCOMPLEX);
            checkFiniteInput<singlecomplex>(A);
            T = A;
            T.ensureSingleOwner();
            computeComplexSchur<singlecomplex>(T, nullptr);
        } else {
            A.promoteType(NLS_DCOMPLEX);
            checkFiniteInput<doublecomplex>(A);
            T = A;
            T.ensureSingleOwner();
            computeComplexSchur<doublecomplex>(T, nullptr);
        }
    } else if (A.dataClass() == NLS_SINGLE) {
        checkFiniteInput<single>(A);
        T = A;
        T.ensureSingleOwner();
        computeRealSchur<single>(T, nullptr);
    } else {
        A.promoteType(NLS_DOUBLE);
        checkFiniteInput<double>(A);
        T = A;
        T.ensureSingleOwner();
        computeRealSchur<double>(T, nullptr);
    }
    promoteRealSchurResult(T);
}
//=============================================================================
void
SchurDecomposition(ArrayOf A, bool asComplex, ArrayOf& U, ArrayOf& T)
{
    checkSchurInput(A);
    Dimensions dimsA = A.dimensions();
    if (A.isEmpty()) {
        U = ArrayOf::empty(dimsA);
        U.promoteType(A.dataClass());
        T = ArrayOf::empty(dimsA);
        T.promoteType(A.dataClass());
        return;
    }
    if (asComplex || A.isComplex()) {
        if (A.dataClass() == NLS_SINGLE || A.dataClass() == NLS_SCOMPLEX) {
            A.promoteType(NLS_SCOMPLEX);
            checkFiniteInput<singlecomplex>(A);
            U = A;
            U.ensureSingleOwner();
            T = A;
            T.ensureSingleOwner();
            computeComplexSchur<singlecomplex>(T, &U);
        } else {
            A.promoteType(NLS_DCOMPLEX);
            checkFiniteInput<doublecomplex>(A);
            U = A;
            U.ensureSingleOwner();
            T = A;
            T.ensureSingleOwner();
            computeComplexSchur<doublecomplex>(T, &U);
        }
    } else if (A.dataClass() == NLS_SINGLE) {
        checkFiniteInput<single>(A);
        U = A;
        U.ensureSingleOwner();
        T = A;
        T.ensureSingleOwner();
        computeRealSchur<single>(T, &U);
    } else {
        A.promoteType(NLS_DOUBLE);
        checkFiniteInput<double>(A);
        U = A;
        U.ensureSingleOwner();
        T = A;
        T.ensureSingleOwner();
        computeRealSchur<double>(T, &U);
    }
    promoteRealSchurResult(U);
    promoteRealSchurResult(T);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
