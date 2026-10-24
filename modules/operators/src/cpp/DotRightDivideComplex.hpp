//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "lapack_eigen_config.hpp"
#include <Eigen/Dense>
#include <cmath>
#include <complex>
#include <limits>
#include "ArrayOf.hpp"
#include "BinaryOperatorsHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
void
normalizeComplexZeroDivisors(std::complex<T>* values, const std::complex<T>* denominators,
    indexType count, bool scalarDenominator = false)
{
    if (scalarDenominator && denominators[0] != std::complex<T>(0)) {
        return;
    }
    // Keep Eigen's vectorized finite quotient, but make zero denominators
    // independent of the packet kernel and platform complex division library.
    for (indexType k = 0; k < count; ++k) {
        const auto denominator = denominators[scalarDenominator ? 0 : k];
        if (denominator == std::complex<T>(0)) {
            const T nan = std::numeric_limits<T>::quiet_NaN();
            values[k] = { nan, nan };
        }
    }
}
//=============================================================================
template <class T>
ArrayOf
scalar_matrix_complex_dotRightDivide(
    NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = B.dimensions();
    indexType Clen = dimsC.elementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);
    T* ptrA = (T*)A.dataPointer();
    T* ptrB = (T*)B.dataPointer();
    T* ptrC = (T*)Cp;
    std::complex<T>* Az = reinterpret_cast<std::complex<T>*>(ptrA);
    std::complex<T>* Bz = reinterpret_cast<std::complex<T>*>(ptrB);
    std::complex<T>* Cz = reinterpret_cast<std::complex<T>*>(ptrC);
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matC(
        (std::complex<T>*)Cz, 1, Clen);
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matB(
        (std::complex<T>*)Bz, 1, Clen);
    matC = Az[0] / matB.array();
    normalizeComplexZeroDivisors(Cz, Bz, Clen);
    return res;
}
//=============================================================================
template <class T>
ArrayOf
matrix_scalar_complex_dotRightDivide(
    NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = A.dimensions();
    indexType Clen = dimsC.elementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);
    T* ptrA = (T*)A.dataPointer();
    T* ptrB = (T*)B.dataPointer();
    T* ptrC = (T*)Cp;
    std::complex<T>* Az = reinterpret_cast<std::complex<T>*>(ptrA);
    std::complex<T>* Bz = reinterpret_cast<std::complex<T>*>(ptrB);
    std::complex<T>* Cz = reinterpret_cast<std::complex<T>*>(ptrC);
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matC(
        (std::complex<T>*)Cz, 1, Clen);
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matA(
        (std::complex<T>*)Az, 1, Clen);
    matC = matA.array() / Bz[0];
    normalizeComplexZeroDivisors(Cz, Bz, Clen, true);
    return res;
}
//=============================================================================
template <class T>
ArrayOf
matrix_matrix_complex_dotRightDivide(
    NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = A.dimensions();
    indexType Clen = dimsC.elementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);
    T* ptrA = (T*)A.dataPointer();
    T* ptrB = (T*)B.dataPointer();
    T* ptrC = (T*)Cp;
    std::complex<T>* Az = reinterpret_cast<std::complex<T>*>(ptrA);
    std::complex<T>* Bz = reinterpret_cast<std::complex<T>*>(ptrB);
    std::complex<T>* Cz = reinterpret_cast<std::complex<T>*>(ptrC);
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matC(
        (std::complex<T>*)Cz, 1, Clen);
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matA(
        (std::complex<T>*)Az, 1, Clen);
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matB(
        (std::complex<T>*)Bz, 1, Clen);
    matC = matA.cwiseQuotient(matB);
    normalizeComplexZeroDivisors(Cz, Bz, Clen);
    return res;
}
//=============================================================================
template <class T>
ArrayOf
row_column_complex_dotRightDivide(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsA = A.dimensions();
    Dimensions dimsB = B.dimensions();
    indexType rows = std::max(dimsA.rows(), dimsB.rows());
    indexType columns = std::max(dimsA.columns(), dimsB.columns());
    Dimensions dimsC(rows, columns);
    indexType Clen = dimsC.elementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);
    T* ptrA = (T*)A.dataPointer();
    T* ptrB = (T*)B.dataPointer();
    T* ptrC = (T*)Cp;
    std::complex<T>* Az = reinterpret_cast<std::complex<T>*>(ptrA);
    std::complex<T>* Bz = reinterpret_cast<std::complex<T>*>(ptrB);
    std::complex<T>* Cz = reinterpret_cast<std::complex<T>*>(ptrC);
    indexType m = 0;
    for (indexType i = 0; i < dimsA.columns(); i++) {
        for (indexType j = 0; j < dimsB.rows(); j++) {
            Cz[m] = Az[i] / Bz[j];
            m++;
        }
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
column_row_complex_dotRightDivide(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsA = A.dimensions();
    Dimensions dimsB = B.dimensions();
    indexType rows = std::max(dimsA.rows(), dimsB.rows());
    indexType columns = std::max(dimsA.columns(), dimsB.columns());
    Dimensions dimsC(rows, columns);
    indexType Clen = dimsC.elementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);
    T* ptrA = (T*)A.dataPointer();
    T* ptrB = (T*)B.dataPointer();
    T* ptrC = (T*)Cp;
    std::complex<T>* Az = reinterpret_cast<std::complex<T>*>(ptrA);
    std::complex<T>* Bz = reinterpret_cast<std::complex<T>*>(ptrB);
    std::complex<T>* Cz = reinterpret_cast<std::complex<T>*>(ptrC);
    indexType m = 0;
    indexType elementCountA = dimsA.elementCount();
    indexType elementCountB = dimsB.elementCount();
    for (indexType i = 0; i < elementCountB; i++) {
        for (indexType j = 0; j < elementCountA; j++) {
            Cz[m] = Az[j] / Bz[i];
            m++;
        }
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
row_matrix_complex_dotRightDivide(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = B.dimensions();
    Dimensions dimsA = A.dimensions();
    indexType Clen = dimsC.elementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);
    T* ptrA = (T*)A.dataPointer();
    T* ptrB = (T*)B.dataPointer();
    T* ptrC = (T*)Cp;
    std::complex<T>* Az = reinterpret_cast<std::complex<T>*>(ptrA);
    std::complex<T>* Bz = reinterpret_cast<std::complex<T>*>(ptrB);
    std::complex<T>* Cz = reinterpret_cast<std::complex<T>*>(ptrC);
    indexType q = 0;
    for (indexType i = 0; i < dimsC.rows(); i++) {
        for (indexType j = 0; j < dimsC.columns(); j++) {
            indexType m = i + j * dimsA.rows();
            Cz[m] = Az[q] / Bz[m];
        }
        q++;
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
matrix_row_complex_dotRightDivide(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = A.dimensions();
    Dimensions dimsB = B.dimensions();

    indexType Clen = dimsC.elementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);
    T* ptrA = (T*)A.dataPointer();
    T* ptrB = (T*)B.dataPointer();
    T* ptrC = (T*)Cp;
    std::complex<T>* Az = reinterpret_cast<std::complex<T>*>(ptrA);
    std::complex<T>* Bz = reinterpret_cast<std::complex<T>*>(ptrB);
    std::complex<T>* Cz = reinterpret_cast<std::complex<T>*>(ptrC);
    indexType q = 0;
    for (indexType i = 0; i < dimsC.rows(); i++) {
        for (indexType j = 0; j < dimsC.columns(); j++) {
            indexType m = i + j * dimsB.rows();
            Cz[m] = Az[m] / Bz[q];
        }
        q++;
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
column_matrix_complex_dotRightDivide(
    NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = B.dimensions();
    Dimensions dimsB = B.dimensions();
    indexType Clen = dimsC.elementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);
    T* ptrA = (T*)A.dataPointer();
    T* ptrB = (T*)B.dataPointer();
    T* ptrC = (T*)Cp;
    std::complex<T>* Az = reinterpret_cast<std::complex<T>*>(ptrA);
    std::complex<T>* Bz = reinterpret_cast<std::complex<T>*>(ptrB);
    std::complex<T>* Cz = reinterpret_cast<std::complex<T>*>(ptrC);
    for (indexType i = 0; i < dimsC.rows(); i++) {
        for (indexType j = 0; j < dimsC.columns(); j++) {
            indexType m = i + j * dimsB.rows();
            Cz[m] = Az[j] / Bz[m];
        }
    }

    return res;
}
//=============================================================================
template <class T>
ArrayOf
matrix_column_complex_dotRightDivide(
    NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = A.dimensions();
    Dimensions dimsA = A.dimensions();
    indexType Clen = dimsC.elementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);
    T* ptrA = (T*)A.dataPointer();
    T* ptrB = (T*)B.dataPointer();
    T* ptrC = (T*)Cp;
    std::complex<T>* Az = reinterpret_cast<std::complex<T>*>(ptrA);
    std::complex<T>* Bz = reinterpret_cast<std::complex<T>*>(ptrB);
    std::complex<T>* Cz = reinterpret_cast<std::complex<T>*>(ptrC);
    for (indexType i = 0; i < dimsC.rows(); i++) {
        for (indexType j = 0; j < dimsC.columns(); j++) {
            indexType m = i + j * dimsA.rows();
            Cz[m] = Az[m] / Bz[j];
        }
    }
    return res;
}
//=============================================================================
// Fast path for complex-numerator / real-divisor element-wise division: scaling
// (a + bi) / c = (a / c) + (b / c)i avoids promoting the divisor to a complex array.
// For finite numerators and finite nonzero divisors it is bit-identical to the complex
// quotient, so the kernels run only when the operands pass
// complexRealDivideOperandsAreClean. Other cases go to the generic path, which defines
// their (NaN / Inf) semantics.
template <class T>
bool
complexRealDivideOperandsAreClean(const std::complex<T>* numerator, indexType numeratorCount,
    const T* divisor, indexType divisorCount)
{
    for (indexType k = 0; k < divisorCount; ++k) {
        if (!std::isfinite(divisor[k]) || divisor[k] == T(0)) {
            return false;
        }
    }
    for (indexType k = 0; k < numeratorCount; ++k) {
        if (!std::isfinite(numerator[k].real()) || !std::isfinite(numerator[k].imag())) {
            return false;
        }
    }
    return true;
}
//=============================================================================
template <class T>
ArrayOf
matrix_matrix_complex_real_dotRightDivide(
    NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    Dimensions dimsC = A.dimensions();
    indexType Clen = dimsC.elementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    const std::complex<T>* Az = reinterpret_cast<const std::complex<T>*>(A.dataPointer());
    const T* Br = reinterpret_cast<const T*>(B.dataPointer());
    std::complex<T>* Cz = reinterpret_cast<std::complex<T>*>(Cp);
    for (indexType k = 0; k < Clen; ++k) {
        Cz[k] = std::complex<T>(Az[k].real() / Br[k], Az[k].imag() / Br[k]);
    }
    return ArrayOf(classDestination, dimsC, Cp, false);
}
//=============================================================================
template <class T>
ArrayOf
matrix_scalar_complex_real_dotRightDivide(
    NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    Dimensions dimsC = A.dimensions();
    indexType Clen = dimsC.elementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    const std::complex<T>* Az = reinterpret_cast<const std::complex<T>*>(A.dataPointer());
    T c = reinterpret_cast<const T*>(B.dataPointer())[0];
    std::complex<T>* Cz = reinterpret_cast<std::complex<T>*>(Cp);
    for (indexType k = 0; k < Clen; ++k) {
        Cz[k] = std::complex<T>(Az[k].real() / c, Az[k].imag() / c);
    }
    return ArrayOf(classDestination, dimsC, Cp, false);
}
//=============================================================================
template <class T>
ArrayOf
scalar_matrix_complex_real_dotRightDivide(
    NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    Dimensions dimsC = B.dimensions();
    indexType Clen = dimsC.elementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    std::complex<T> a = reinterpret_cast<const std::complex<T>*>(A.dataPointer())[0];
    const T* Br = reinterpret_cast<const T*>(B.dataPointer());
    std::complex<T>* Cz = reinterpret_cast<std::complex<T>*>(Cp);
    for (indexType k = 0; k < Clen; ++k) {
        Cz[k] = std::complex<T>(a.real() / Br[k], a.imag() / Br[k]);
    }
    return ArrayOf(classDestination, dimsC, Cp, false);
}
//=============================================================================
template <class T>
bool
complex_real_dotRightDivide_scaling(
    NelsonType classDestination, const ArrayOf& A, const ArrayOf& B, ArrayOf& result)
{
    bool matrixMatrix = A.dimensions().equals(B.dimensions());
    bool matrixScalar = !matrixMatrix && B.isScalar();
    bool scalarMatrix = !matrixMatrix && !matrixScalar && A.isScalar();
    if (!matrixMatrix && !matrixScalar && !scalarMatrix) {
        // Broadcasting shapes (row/column, row/matrix, ...) keep the generic path.
        return false;
    }
    const std::complex<T>* Az = reinterpret_cast<const std::complex<T>*>(A.dataPointer());
    const T* Br = reinterpret_cast<const T*>(B.dataPointer());
    indexType numeratorCount = A.isScalar() ? 1 : A.dimensions().elementCount();
    indexType divisorCount = B.isScalar() ? 1 : B.dimensions().elementCount();
    if (!complexRealDivideOperandsAreClean<T>(Az, numeratorCount, Br, divisorCount)) {
        return false;
    }
    if (matrixMatrix) {
        result = matrix_matrix_complex_real_dotRightDivide<T>(classDestination, A, B);
    } else if (matrixScalar) {
        result = matrix_scalar_complex_real_dotRightDivide<T>(classDestination, A, B);
    } else {
        result = scalar_matrix_complex_real_dotRightDivide<T>(classDestination, A, B);
    }
    return true;
}
//=============================================================================
template <class T>
ArrayOf
complex_dotRightDivide(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    return binaryOperator2DDispatch(classDestination, A, B, matrix_matrix_complex_dotRightDivide<T>,
        scalar_matrix_complex_dotRightDivide<T>, matrix_scalar_complex_dotRightDivide<T>,
        matrix_matrix_complex_dotRightDivide<T>, row_column_complex_dotRightDivide<T>,
        column_row_complex_dotRightDivide<T>, row_matrix_complex_dotRightDivide<T>,
        matrix_row_complex_dotRightDivide<T>, column_matrix_complex_dotRightDivide<T>,
        matrix_column_complex_dotRightDivide<T>,
        [](const ArrayOf& lhs, const ArrayOf& rhs, NelsonType destinationClass) {
            return binaryOperatorBroadcastComplex<T>(lhs, rhs, destinationClass, "./",
                [](std::complex<T> a, std::complex<T> b) { return a / b; });
        });
}
//=============================================================================
} // namespace Nelson
//=============================================================================
