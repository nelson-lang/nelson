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
// The dense matrix product below calls the Fortran BLAS xgemm directly (via
// LapackPrototypes), so keep Eigen's own products native in this translation
// unit and let it fall back to its inlined kernel for the small sizes.
#undef EIGEN_USE_MKL_VML
#undef EIGEN_USE_MKL
#undef EIGEN_USE_BLAS
#include "LapackPrototypes.hpp"
#include <Eigen/Dense>
#include "MatrixMultiplication.hpp"
#include "IntegerOperations.hpp"
#include "MatrixCheck.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "NewWithException.hpp"
#include "i18n.hpp"
#include "IntegerCompatibilityHelpers.hpp"
#include "SparseOperatorsHelpers.hpp"
#include "Decomplexify.hpp"
#include <complex>
#include <limits>
//=============================================================================
namespace Nelson {
//=============================================================================
// Dense matrix product through the threaded BLAS xgemm (defined at the end of
// this file); C(m x n) = A(m x k) * B(k x n), column-major, no transpose.
static bool
useBlasGemm(size_t m, size_t k, size_t n);
static void
blasGemm(lapack_int m, lapack_int n, lapack_int k, const double* A, const double* B, double* C);
static void
blasGemm(lapack_int m, lapack_int n, lapack_int k, const float* A, const float* B, float* C);
static void
blasGemm(lapack_int m, lapack_int n, lapack_int k, const std::complex<double>* A,
    const std::complex<double>* B, std::complex<double>* C);
static void
blasGemm(lapack_int m, lapack_int n, lapack_int k, const std::complex<float>* A,
    const std::complex<float>* B, std::complex<float>* C);
//=============================================================================
static bool
isSizeMismatch(const ArrayOf& A, const ArrayOf& B)
{
    if (!A.isScalar() && !B.isScalar()) {
        return A.columns() != B.rows();
    }
    bool isVectorOrScalar = ((A.isVector() && B.isScalar()) || (B.isVector() && A.isScalar())
        || (A.isRowVector() && B.isColumnVector()) || (B.isRowVector() && A.isColumnVector())
        || (A.isScalar() || B.isScalar()));
    return !isVectorOrScalar;
}
//=============================================================================
static bool
getEmptyMatrixProductDimensions(const ArrayOf& A, const ArrayOf& B, Dimensions& dimsC)
{
    Dimensions dimsA = A.dimensions();
    Dimensions dimsB = B.dimensions();
    dimsA.simplify();
    dimsB.simplify();
    if (A.isScalar() || B.isScalar()) {
        return false;
    }
    if (dimsA.length() > 2 || dimsB.length() > 2) {
        return false;
    }
    if (dimsA[1] != dimsB[0]) {
        return false;
    }
    if (dimsA[0] != 0 && dimsA[1] != 0 && dimsB[0] != 0 && dimsB[1] != 0) {
        return false;
    }
    dimsC = Dimensions(dimsA[0], dimsB[1]);
    return true;
}
//=============================================================================
static ArrayOf
zeroMatrixProductResult(NelsonType dataClass, const Dimensions& dimsC)
{
    if (dimsC.elementCount() == 0) {
        return ArrayOf(dataClass, dimsC, static_cast<void*>(nullptr), false);
    }
    void* ptrC = ArrayOf::allocateArrayOf(dataClass, dimsC.elementCount(), stringVector(), true);
    return ArrayOf(dataClass, dimsC, ptrC, false);
}
//=============================================================================
template <class T>
static ArrayOf
zeroSparseMatrixProductResult(NelsonType dataClass, const Dimensions& dimsC)
{
    auto* sparseResult = new NelsonSparseMatrix<T>(dimsC.rows(), dimsC.columns());
    return makeSparseOperatorResult<T>(dataClass, dimsC, sparseResult);
}
//=============================================================================
template <class T>
static Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>
denseSparseMatrixToDenseEigenMatrix(const ArrayOf& A)
{
    Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> dense(A.rows(), A.columns());
    if (A.isSparse()) {
        dense.setZero();
        const auto* sparseA = getSparseMatrixPointer<T>(A);
        for (signedIndexType col = 0; col < sparseA->outerSize(); ++col) {
            for (typename NelsonSparseMatrix<T>::InnerIterator it(*sparseA, col); it; ++it) {
                dense(it.row(), it.col()) = it.value();
            }
        }
    } else {
        const T* ptrA = static_cast<const T*>(A.dataPointer());
        for (indexType col = 0; col < A.columns(); ++col) {
            for (indexType row = 0; row < A.rows(); ++row) {
                dense(static_cast<signedIndexType>(row), static_cast<signedIndexType>(col))
                    = ptrA[row + col * A.rows()];
            }
        }
    }
    return dense;
}
//=============================================================================
template <class T>
static ArrayOf
denseSparseMatrixMultiplication(NelsonType dataClass, const ArrayOf& A, const ArrayOf& B)
{
    if (A.isScalar() && B.isSparse()) {
        const T* ptrA = static_cast<const T*>(A.dataPointer());
        return scalarSparseOperatorProduct<T>(dataClass, B.dimensions(), B, ptrA[0]);
    }
    if (A.isScalar() && A.isSparse()) {
        ArrayOf sparseB(B);
        sparseB.makeSparse();
        return scalarSparseOperatorProduct<T>(
            dataClass, B.dimensions(), sparseB, getSparseScalarValue<T>(A));
    }
    if (B.isScalar() && A.isSparse()) {
        const T* ptrB = static_cast<const T*>(B.dataPointer());
        return scalarSparseOperatorProduct<T>(dataClass, A.dimensions(), A, ptrB[0]);
    }
    if (B.isScalar() && B.isSparse()) {
        ArrayOf sparseA(A);
        sparseA.makeSparse();
        return scalarSparseOperatorProduct<T>(
            dataClass, A.dimensions(), sparseA, getSparseScalarValue<T>(B));
    }
    if (!A.is2D() || !B.is2D()) {
        Error(L"Nelson:expected2D", L"%s must be 2-D.", L"Input arguments");
    }
    if (isSizeMismatch(A, B)) {
        Error("Nelson:innerdim", "Incorrect dimensions for matrix multiplication.");
    }
    Dimensions emptyDimsC;
    if (getEmptyMatrixProductDimensions(A, B, emptyDimsC)) {
        return zeroMatrixProductResult(dataClass, emptyDimsC);
    }
    // Exactly one operand is sparse here. Eigen's sparse*dense product is O(nnz);
    // densifying the sparse operand would cost O(rows*columns) in time and memory.
    Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> denseC;
    if (A.isSparse()) {
        const NelsonSparseMatrix<T>* sparseA = getSparseMatrixPointer<T>(A);
        Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> denseB
            = denseSparseMatrixToDenseEigenMatrix<T>(B);
        denseC = (*sparseA) * denseB;
    } else {
        Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> denseA
            = denseSparseMatrixToDenseEigenMatrix<T>(A);
        const NelsonSparseMatrix<T>* sparseB = getSparseMatrixPointer<T>(B);
        denseC = denseA * (*sparseB);
    }
    Dimensions dimsC(denseC.rows(), denseC.cols());
    T* ptrC = static_cast<T*>(ArrayOf::allocateArrayOf(dataClass, dimsC.elementCount()));
    for (indexType col = 0; col < dimsC.columns(); ++col) {
        for (indexType row = 0; row < dimsC.rows(); ++row) {
            ptrC[row + col * dimsC.rows()]
                = denseC(static_cast<signedIndexType>(row), static_cast<signedIndexType>(col));
        }
    }
    return ArrayOf(dataClass, dimsC, ptrC, false);
}
//=============================================================================
template <class T>
static ArrayOf
sparseMatrixMultiplication(NelsonType dataClass, const ArrayOf& A, const ArrayOf& B)
{
    if (!A.isSparse() || !B.isSparse()) {
        return denseSparseMatrixMultiplication<T>(dataClass, A, B);
    }
    if (A.isScalar()) {
        return scalarSparseOperatorProduct<T>(
            dataClass, B.dimensions(), B, getSparseScalarValue<T>(A));
    }
    if (B.isScalar()) {
        return scalarSparseOperatorProduct<T>(
            dataClass, A.dimensions(), A, getSparseScalarValue<T>(B));
    }
    if (!A.is2D() || !B.is2D()) {
        Error(L"Nelson:expected2D", L"%s must be 2-D.", L"Input arguments");
    }
    if (isSizeMismatch(A, B)) {
        Error("Nelson:innerdim", "Incorrect dimensions for matrix multiplication.");
    }
    Dimensions emptyDimsC;
    if (getEmptyMatrixProductDimensions(A, B, emptyDimsC)) {
        return zeroSparseMatrixProductResult<T>(dataClass, emptyDimsC);
    }
    return matrixSparseOperatorMultiplication<T>(dataClass, A.rows(), B.columns(), A, B);
}
//=============================================================================
static ArrayOf
sparseMatrixMultiplication(const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    needToOverload = false;
    if ((!A.isSparse() && !B.isSparse()) || A.dataClass() != B.dataClass()) {
        needToOverload = true;
        return {};
    }
    switch (A.dataClass()) {
    case NLS_LOGICAL:
        return sparseMatrixMultiplication<logical>(NLS_LOGICAL, A, B);
    case NLS_DOUBLE:
        return sparseMatrixMultiplication<double>(NLS_DOUBLE, A, B);
    case NLS_SINGLE:
        return sparseMatrixMultiplication<single>(NLS_SINGLE, A, B);
    case NLS_DCOMPLEX:
        return sparseMatrixMultiplication<doublecomplex>(NLS_DCOMPLEX, A, B);
    case NLS_SCOMPLEX:
        return sparseMatrixMultiplication<singlecomplex>(NLS_SCOMPLEX, A, B);
    default:
        needToOverload = true;
        return {};
    }
}
//=============================================================================
template <class T>
static ArrayOf
real_mtimes(NelsonType currentClass, const ArrayOf& A, const ArrayOf& B)
{
    Dimensions Cdim;
    if (A.isScalar() && B.isScalar()) {
        T* ptrC = (T*)ArrayOf::allocateArrayOf(currentClass, 1);
        if (A.dataClass() == currentClass && B.dataClass() == currentClass) {
            ptrC[0] = ((T*)A.dataPointer())[0] * ((T*)B.dataPointer())[0];
        } else {
            ArrayOf _A(A);
            _A.promoteType(currentClass);
            ArrayOf _B(B);
            _B.promoteType(currentClass);
            ptrC[0] = ((T*)_A.dataPointer())[0] * ((T*)_B.dataPointer())[0];
        }
        return ArrayOf(currentClass, Dimensions(1, 1), ptrC, false);
    }
    if (A.isVector() && B.isScalar()) {
        Cdim = A.dimensions();
    } else if (B.isVector() && A.isScalar()) {
        Cdim = B.dimensions();
    } else if ((A.isRowVector() && B.isColumnVector()) || (B.isRowVector() && A.isColumnVector())) {
        Cdim[0] = A.rows();
        Cdim[1] = B.columns();
    } else {
        if (A.isScalar()) {
            Cdim = B.dimensions();
        } else if (B.isScalar()) {
            Cdim = A.dimensions();
        } else {
            Cdim[0] = A.rows();
            Cdim[1] = B.columns();
        }
    }
    ArrayOf _A(A);
    ArrayOf _B(B);
    _A.promoteType(currentClass);
    _B.promoteType(currentClass);

    indexType Clen = Cdim.elementCount();
    if (Clen == 0) {
        return zeroMatrixProductResult(currentClass, Cdim);
    }
    void* Cp = new_with_exception<T>(Clen, false);
    T* ptrC = (T*)Cp;
    size_t mC = Cdim.rows();
    size_t nC = Cdim.columns();
    Eigen::Map<Eigen::Matrix<T, -1, -1>> matC((T*)Cp, mC, nC);
    Dimensions dimA = A.dimensions();
    size_t mA = dimA.rows();
    size_t nA = dimA.columns();
    Dimensions dimB = B.dimensions();
    size_t mB = dimB.rows();
    size_t nB = dimB.columns();
    if (A.isScalar()) {
        T* ptrA = (T*)_A.dataPointer();
        T* ptrB = (T*)_B.dataPointer();
        ompIndexType elementCount = (ompIndexType)dimB.elementCount();
#if WITH_OPENMP
        OMP_PARALLEL_FOR_LOOP(elementCount)
        for (ompIndexType k = 0; k < elementCount; k++) {
            ptrC[k] = ptrA[0] * ptrB[k];
        }
#else
        for (ompIndexType k = 0; k < elementCount; k++) {
            ptrC[k] = ptrA[0] * ptrB[k];
        }
#endif
    } else if (B.isScalar()) {
        T* ptrA = (T*)_A.dataPointer();
        T* ptrB = (T*)_B.dataPointer();
        ompIndexType elementCount = (ompIndexType)dimA.elementCount();
#if WITH_OPENMP
        OMP_PARALLEL_FOR_LOOP(elementCount)
        for (ompIndexType k = 0; k < elementCount; k++) {
            ptrC[k] = ptrA[k] * ptrB[0];
        }
#else
        for (ompIndexType k = 0; k < elementCount; k++) {
            ptrC[k] = ptrA[k] * ptrB[0];
        }
#endif
    } else {
        if (useBlasGemm(mA, nA, nB)) {
            blasGemm((int)mA, (int)nB, (int)nA, (const T*)_A.dataPointer(),
                (const T*)_B.dataPointer(), ptrC);
        } else {
            Eigen::Map<Eigen::Matrix<T, -1, -1>> matA((T*)_A.dataPointer(), mA, nA);
            Eigen::Map<Eigen::Matrix<T, -1, -1>> matB((T*)_B.dataPointer(), mB, nB);
            matC = matA * matB;
        }
    }
    return ArrayOf(currentClass, Cdim, Cp, false);
}
//=============================================================================
template <class T>
static ArrayOf
integer_mtimes(const ArrayOf& A, const ArrayOf& B)
{
    Dimensions Cdim;
    if (A.isVector() && B.isScalar()) {
        Cdim = A.dimensions();
    } else if (B.isVector() && A.isScalar()) {
        Cdim = B.dimensions();
    } else if ((A.isRowVector() && B.isColumnVector()) || (B.isRowVector() && A.isColumnVector())) {
        Cdim[0] = A.rows();
        Cdim[1] = B.columns();
    } else {
        if (A.isScalar()) {
            Cdim = B.dimensions();
        } else if (B.isScalar()) {
            Cdim = A.dimensions();
        } else {
            Cdim[0] = A.rows();
            Cdim[1] = B.columns();
        }
    }
    indexType Clen = Cdim.elementCount();
    if (Clen == 0) {
        return zeroMatrixProductResult(A.dataClass(), Cdim);
    }
    void* Cp = new_with_exception<T>(Clen, false);
    size_t mC = Cdim.rows();
    size_t nC = Cdim.columns();
    Dimensions dimA = A.dimensions();
    size_t mA = dimA.rows();
    size_t nA = dimA.columns();
    Dimensions dimB = B.dimensions();
    size_t mB = dimB.rows();
    size_t nB = dimB.columns();
    T* ptrA = (T*)A.dataPointer();
    T* ptrB = (T*)B.dataPointer();
    T* ptrC = (T*)Cp;
    if (A.isScalar()) {
        ompIndexType elementCountB = (ompIndexType)dimB.elementCount();
        OMP_PARALLEL_FOR_LOOP(elementCountB)
        for (ompIndexType k = 0; k < elementCountB; k++) {
            ptrC[k] = scalar_scalar_integer_times<T>(ptrA[0], ptrB[k]);
        }
    } else if (B.isScalar()) {
        ompIndexType elementCountA = (ompIndexType)dimA.elementCount();
        OMP_PARALLEL_FOR_LOOP(elementCountA)
        for (ompIndexType k = 0; k < elementCountA; k++) {
            ptrC[k] = scalar_scalar_integer_times<T>(ptrA[k], ptrB[0]);
        }
    } else {
        Error(L"Nelson:operators:nonScalarInput", L"At least one input argument must be scalar.");
    }
    return ArrayOf(A.dataClass(), Cdim, Cp, false);
}
//=============================================================================
template <class T>
static ArrayOf
complex_mtimes(NelsonType currentClass, const ArrayOf& A, const ArrayOf& B)
{
    Dimensions Cdim;
    ArrayOf AA = A;
    ArrayOf BB = B;
    AA.promoteType(currentClass);
    BB.promoteType(currentClass);
    if (A.isVector() && B.isScalar()) {
        Cdim = A.dimensions();
    } else if (B.isVector() && A.isScalar()) {
        Cdim = B.dimensions();
    } else if ((A.isRowVector() && B.isColumnVector()) || (B.isRowVector() && A.isColumnVector())) {
        Cdim[0] = A.rows();
        Cdim[1] = B.columns();
    } else {
        if (A.isScalar()) {
            Cdim = B.dimensions();
        } else if (B.isScalar()) {
            Cdim = A.dimensions();
        } else {
            Cdim[0] = A.rows();
            Cdim[1] = B.columns();
        }
    }
    indexType Clen = Cdim.elementCount();
    if (Clen == 0) {
        return zeroMatrixProductResult(currentClass, Cdim);
    }
    void* Cp = new_with_exception<T>(Clen * 2, false);
    std::complex<T>* Cz = reinterpret_cast<std::complex<T>*>(Cp);
    size_t mC = Cdim.rows();
    size_t nC = Cdim.columns();
    Eigen::Map<Eigen::Matrix<std::complex<T>, -1, -1>> matC(Cz, mC, nC);
    Dimensions dimA = A.dimensions();
    size_t mA = dimA.rows();
    size_t nA = dimA.columns();
    Dimensions dimB = B.dimensions();
    size_t mB = dimB.rows();
    size_t nB = dimB.columns();
    if (A.isScalar() && B.isScalar()) {
        std::complex<T>* Az = reinterpret_cast<std::complex<T>*>((T*)AA.dataPointer());
        std::complex<T> cxa = Az[0];
        std::complex<T>* Bz = reinterpret_cast<std::complex<T>*>((T*)BB.dataPointer());
        std::complex<T> cxb = Bz[0];
        if ((cxa.real() == 0.) && (cxa.imag() == 0.) || (cxb.real() == 0.) && (cxb.imag() == 0.)) {
            T* pd = (T*)Cp;
            delete[] pd;
            pd = nullptr;
            if (currentClass == NLS_DCOMPLEX) {
                return ArrayOf::doubleScalar(0);
            }
            return ArrayOf::singleScalar(0);
        }
        Cz[0] = cxa * cxb;

    } else if (A.isScalar()) {
        std::complex<T>* Az = reinterpret_cast<std::complex<T>*>((T*)AA.dataPointer());
        std::complex<T>* Bz = reinterpret_cast<std::complex<T>*>((T*)BB.dataPointer());
        if ((Az[0].real() == 0.) && (Az[0].imag() == 0.)) {
            T* pd = (T*)Cp;
            delete[] pd;
            pd = nullptr;
            Cp = ArrayOf::allocateArrayOf(NLS_DOUBLE, Cdim.elementCount(), stringVector(), true);
            return ArrayOf(NLS_DOUBLE, Cdim, Cp);
        }
        ompIndexType elementCount = (ompIndexType)dimB.elementCount();
#if WITH_OPENMP
        OMP_PARALLEL_FOR_LOOP(elementCount)
        for (ompIndexType k = 0; k < elementCount; k++) {
            Cz[k] = Az[0] * Bz[k];
        }
#else
        for (ompIndexType k = 0; k < elementCount; k++) {
            Cz[k] = Az[0] * Bz[k];
        }
#endif

    } else if (B.isScalar()) {
        std::complex<T>* Bz = reinterpret_cast<std::complex<T>*>((T*)BB.dataPointer());
        std::complex<T>* Az = reinterpret_cast<std::complex<T>*>((T*)AA.dataPointer());
        Eigen::Map<Eigen::Matrix<std::complex<T>, -1, -1>> matA(Az, mA, nA);
        matC = matA.array() * Bz[0];
        if ((Bz[0].real() == 0.) && (Bz[0].imag() == 0.)) {
            T* pd = (T*)Cp;
            delete[] pd;
            pd = nullptr;
            Cp = ArrayOf::allocateArrayOf(A.dataClass(), Cdim.elementCount(), stringVector(), true);
            return ArrayOf(A.dataClass(), Cdim, Cp);
        }
        ompIndexType elementCount = (ompIndexType)dimA.elementCount();
#if WITH_OPENMP
        OMP_PARALLEL_FOR_LOOP(elementCount)
        for (ompIndexType k = 0; k < elementCount; k++) {
            Cz[k] = Az[k] * Bz[0];
        }
#else
        for (ompIndexType k = 0; k < elementCount; k++) {
            Cz[k] = Az[k] * Bz[0];
        }
#endif

    } else {
        std::complex<T>* Az = reinterpret_cast<std::complex<T>*>((T*)AA.dataPointer());
        std::complex<T>* Bz = reinterpret_cast<std::complex<T>*>((T*)BB.dataPointer());
        if (useBlasGemm(mA, nA, nB)) {
            blasGemm((int)mA, (int)nB, (int)nA, Az, Bz, Cz);
        } else {
            Eigen::Map<Eigen::Matrix<std::complex<T>, -1, -1>> matA(Az, mA, nA);
            Eigen::Map<Eigen::Matrix<std::complex<T>, -1, -1>> matB(Bz, mB, nB);
            matC = matA * matB;
        }
    }
    return ArrayOf(currentClass, Cdim, Cp, false);
}
//=============================================================================
template <class T>
ArrayOf
T_mtimes_T(NelsonType realClass, NelsonType complexClass, const ArrayOf& A, const ArrayOf& B)
{
    Dimensions dimsA = A.dimensions();
    Dimensions dimsB = B.dimensions();
    bool withScalar = A.isScalar() || B.isScalar();
    if ((dimsA.length() > 2 || dimsB.length() > 2) && !withScalar) {
        Error(L"Nelson:expected2D", L"%s must be 2-D.", L"Input arguments");
    }
    // a product with an empty operand is empty or all zeros: it is real
    Dimensions emptyDimsC;
    if (getEmptyMatrixProductDimensions(A, B, emptyDimsC)) {
        return zeroMatrixProductResult(realClass, emptyDimsC);
    }
    if (A.isEmpty() || B.isEmpty()) {
        // [] * 2
        // 2 * []
        if (A.isScalar() || B.isScalar()) {
            return zeroMatrixProductResult(realClass, A.isScalar() ? dimsB : dimsA);
        }
        dimsA.simplify();
        dimsB.simplify();
        // [] * [] = []
        if (A.isEmpty(true) && B.isEmpty(true)) {
            return zeroMatrixProductResult(realClass, dimsA);
        }
        // [](mx0) * [](0xn) = 0(mxn)
        if ((dimsA[1] == 0) && (dimsB[0] == 0) && (dimsB.length() < 3)) {
            return zeroMatrixProductResult(realClass, Dimensions(dimsA[0], dimsB[1]));
        }
        // [](0xm) * M(mxn) = [](0xn)
        if ((dimsA[0] == 0) && (dimsA[1] == dimsB[0])) {
            return zeroMatrixProductResult(realClass, Dimensions(0, dimsB[1]));
        }
        // M(mxn) * [](nx0) = [](mx0)
        if ((dimsB[0] == dimsA[1]) && (dimsB.length() < 3)) {
            return zeroMatrixProductResult(realClass, Dimensions(dimsA[0], 0));
        }
        Error("Nelson:innerdim", "Incorrect dimensions for matrix multiplication.");
    }
    if ((!A.is2D() || !B.is2D()) && !withScalar) {
        Error(L"Nelson:expected2D", L"%s must be 2-D.", L"Input arguments");
    }
    if (isSizeMismatch(A, B)) {
        Error("Nelson:innerdim", "Incorrect dimensions for matrix multiplication.");
    }
    if (A.isEmpty()) {
        size_t mA = dimsA.rows();
        size_t nA = dimsA.columns();
        if (mA == nA) {
            if (B.isScalar()) {
                // [] * X returns []
                return { B.dataClass() };
            }
            RaiseError(
                resolveMessageTemplate(L"Nelson:operators:usingOperator", L"using operator '*'")
                    + L" \n "
                    + Nelson::resolveMessageTemplate(L"Nelson:operators:matrixDimensionsAgree",
                        L"Matrix dimensions must agree."),
                L"Nelson:dimagree");
        }
    }
    if (A.isComplex() || B.isComplex()) {
        ArrayOf res = complex_mtimes<T>(complexClass, A, B);
        if (res.allReal()) {
            res.promoteType(realClass);
        }
        return res;
    }
    return real_mtimes<T>(realClass, A, B);
}
//=============================================================================
template <class T>
ArrayOf
integer_mtimes_integer(const ArrayOf& A, const ArrayOf& B)
{
    Dimensions dimsA = A.dimensions();
    Dimensions dimsB = B.dimensions();
    if (dimsA.length() > 2 || dimsB.length() > 2) {
        Error(L"Nelson:expected2D", L"%s must be 2-D.", L"Input arguments");
    }
    Dimensions emptyDimsC;
    if (getEmptyMatrixProductDimensions(A, B, emptyDimsC)) {
        return zeroMatrixProductResult(A.dataClass(), emptyDimsC);
    }
    if (A.isEmpty() || B.isEmpty()) {
        dimsA.simplify();
        dimsB.simplify();
        // [] * 2
        // 2 * []
        if (A.isScalar() || B.isScalar()) {
            if (A.isScalar()) {
                return ArrayOf(B);
            }
            return ArrayOf(A);
        }
        // [] * [] = []
        if (A.isEmpty(true) && B.isEmpty(true)) {
            ArrayOf res = ArrayOf::empty(dimsA);
            res.promoteType(A.dataClass());
            return res;
        }
        // [](mx0) * [](0xn) = 0(mxn)
        if ((dimsA[1] == 0) && (dimsB[0] == 0) && (dimsB.length() < 3)) {
            Dimensions dimsC(dimsA[0], dimsB[1]);
            T* pT = (T*)ArrayOf::allocateArrayOf(
                A.dataClass(), dimsC.elementCount(), stringVector(), true);
            return ArrayOf(A.dataClass(), dimsC, pT, false);
        }
        // [](0xm) * M(mxn) = [](0xn)
        if ((dimsA[0] == 0) && (dimsA[1] == dimsB[0])) {
            Dimensions dimsC(0, dimsB[1]);
            ArrayOf res = ArrayOf::empty(dimsC);
            res.promoteType(A.dataClass());
            return res;
        }
        // M(mxn) * [](nx0) = [](mx0)
        if ((dimsB[0] == dimsA[1]) && (dimsB.length() < 3)) {
            Dimensions dimsC(dimsA[0], 0);
            ArrayOf res = ArrayOf::empty(dimsC);
            res.promoteType(A.dataClass());
            return res;
        }
        Error("Nelson:innerdim", "Incorrect dimensions for matrix multiplication.");
    }
    if (!A.is2D() || !B.is2D()) {
        Error(L"Nelson:expected2D", L"%s must be 2-D.", L"Input arguments");
    }
    if (isSizeMismatch(A, B)) {
        Error("Nelson:innerdim", "Incorrect dimensions for matrix multiplication.");
    }
    if (A.isEmpty()) {
        size_t mA = dimsA.rows();
        size_t nA = dimsA.columns();
        if (mA == nA) {
            if (B.isScalar()) {
                // [] * X returns []
                return { B.dataClass() };
            }
            RaiseError(
                resolveMessageTemplate(L"Nelson:operators:usingOperator", L"using operator '*'")
                    + L" \n "
                    + Nelson::resolveMessageTemplate(L"Nelson:operators:matrixDimensionsAgree",
                        L"Matrix dimensions must agree."),
                L"Nelson:dimagree");
        }
    }
    return integer_mtimes<T>(A, B);
}
//=============================================================================
ArrayOf
matrixMultiplication(const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    if (A.isSparse() || B.isSparse()) {
        ArrayOf res = sparseMatrixMultiplication(A, B, needToOverload);
        // an empty product by a scalar has no imaginary part
        bool scaledEmpty = !needToOverload && res.isEmpty() && (A.isScalar() || B.isScalar());
        return scaledEmpty ? decomplexify(res) : res;
    }

    if (A.isDoubleClass() && B.isDoubleClass()) {
        return T_mtimes_T<double>(NLS_DOUBLE, NLS_DCOMPLEX, A, B);
    }
    if (A.isSingleClass() && B.isSingleClass()) {
        return T_mtimes_T<single>(NLS_SINGLE, NLS_SCOMPLEX, A, B);
    }
    if (A.isSingleClass() && B.isDoubleClass()) {
        return T_mtimes_T<single>(NLS_SINGLE, NLS_SCOMPLEX, A, B);
    }
    if (A.isDoubleClass() && B.isSingleClass()) {
        return T_mtimes_T<single>(NLS_SINGLE, NLS_SCOMPLEX, A, B);
    }
    if (A.dataClass() == B.dataClass()) {
        switch (A.dataClass()) {
        case NLS_UINT8:
            return integer_mtimes_integer<uint8>(A, B);
        case NLS_INT8:
            return integer_mtimes_integer<int8>(A, B);
        case NLS_UINT16:
            return integer_mtimes_integer<uint16>(A, B);
        case NLS_INT16:
            return integer_mtimes_integer<int16>(A, B);
        case NLS_UINT32:
            return integer_mtimes_integer<uint32>(A, B);
        case NLS_INT32:
            return integer_mtimes_integer<int32>(A, B);
        case NLS_UINT64:
            return integer_mtimes_integer<uint64>(A, B);
        case NLS_INT64:
            return integer_mtimes_integer<int64>(A, B);
        default:
            needToOverload = true;
            break;
        }
    } else {
        if (A.isIntegerType()) {
            bool isCompatible = isScalarDoubleOrChar(B);
            if (!isCompatible) {
                Error(L"Nelson:operators:integerClassMismatch",
                    L"Integers can only be combined with integers of the same class, or scalar "
                    L"doubles.");
            }
            if (B.dataClass() == NLS_CHAR) {
                ArrayOf BB = B;
                BB.promoteType(A.dataClass());
                return matrixMultiplication(A, BB, needToOverload);
            }
            if (B.isComplex()) {
                Error(L"Nelson:operators:complexIntegerNotAllowed",
                    L"Complex integer not allowed for arithmetic operator *");
            }
            ArrayOf AA = A;
            AA.promoteType(B.dataClass());
            ArrayOf res = matrixMultiplication(AA, B, needToOverload);
            if (!needToOverload) {
                res.promoteType(A.dataClass());
            }
            return res;
        } else if (B.isIntegerType()) {
            bool isCompatible = isScalarDoubleOrChar(A);
            if (!isCompatible) {
                Error(L"Nelson:operators:integerClassMismatch",
                    L"Integers can only be combined with integers of the same class, or scalar "
                    L"doubles.");
            }
            if (A.dataClass() == NLS_CHAR) {
                ArrayOf AA = A;
                AA.promoteType(B.dataClass());
                return matrixMultiplication(AA, B, needToOverload);
            }
            if (A.isComplex()) {
                Error(L"Nelson:operators:complexIntegerNotAllowed",
                    L"Complex integer not allowed for arithmetic operator *");
            }
            ArrayOf BB = B;
            BB.promoteType(A.dataClass());
            ArrayOf res = matrixMultiplication(A, BB, needToOverload);
            if (!needToOverload) {
                res.promoteType(B.dataClass());
            }
            return res;
        }
    }
    needToOverload = true;
    return {};
}
//=============================================================================
// Below this flop count Eigen's inlined kernel beats the BLAS call overhead;
// above it the threaded BLAS xgemm (MKL / OpenBLAS) wins decisively. Also
// guards the lapack_int cast used for the BLAS (LP64) dimensions.
static bool
useBlasGemm(size_t m, size_t k, size_t n)
{
    const size_t maxInt = static_cast<size_t>((std::numeric_limits<lapack_int>::max)());
    return m >= 1 && k >= 1 && n >= 1 && m <= maxInt && k <= maxInt && n <= maxInt
        && (m * k * n) >= 65536ULL;
}
//=============================================================================
static void
blasGemm(lapack_int m, lapack_int n, lapack_int k, const double* A, const double* B, double* C)
{
    const char no = 'N';
    const double one = 1.0;
    const double zero = 0.0;
    BLAS_dgemm(&no, &no, &m, &n, &k, &one, A, &m, B, &k, &zero, C, &m);
}
//=============================================================================
static void
blasGemm(lapack_int m, lapack_int n, lapack_int k, const float* A, const float* B, float* C)
{
    const char no = 'N';
    const float one = 1.0F;
    const float zero = 0.0F;
    BLAS_sgemm(&no, &no, &m, &n, &k, &one, A, &m, B, &k, &zero, C, &m);
}
//=============================================================================
static void
blasGemm(lapack_int m, lapack_int n, lapack_int k, const std::complex<double>* A,
    const std::complex<double>* B, std::complex<double>* C)
{
    const char no = 'N';
    const std::complex<double> one(1.0, 0.0);
    const std::complex<double> zero(0.0, 0.0);
    BLAS_zgemm(&no, &no, &m, &n, &k, &one, A, &m, B, &k, &zero, C, &m);
}
//=============================================================================
static void
blasGemm(lapack_int m, lapack_int n, lapack_int k, const std::complex<float>* A,
    const std::complex<float>* B, std::complex<float>* C)
{
    const char no = 'N';
    const std::complex<float> one(1.0F, 0.0F);
    const std::complex<float> zero(0.0F, 0.0F);
    BLAS_cgemm(&no, &no, &m, &n, &k, &one, A, &m, B, &k, &zero, C, &m);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
