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
#include <algorithm>
#include <cmath>
#include <complex>
#include <limits>
#include "ArrayOf.hpp"
#include "BinaryOperatorsHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Vectorized quotient by blocks; the special ones (real or imaginary divisor, Inf or NaN
// parts) are redone with complexQuotientSpecial while the block is still in cache.
template <class T>
void
blockedComplexQuotient(std::complex<T>* values, const std::complex<T>* numerators,
    bool scalarNumerator, const std::complex<T>* denominators, bool scalarDenominator,
    indexType count)
{
    using ArrayMap = Eigen::Map<Eigen::Array<std::complex<T>, Eigen::Dynamic, 1>>;
    using ConstArrayMap = Eigen::Map<const Eigen::Array<std::complex<T>, Eigen::Dynamic, 1>>;
    const indexType blockSize = 4096;
    for (indexType start = 0; start < count; start += blockSize) {
        indexType len = std::min(blockSize, count - start);
        ArrayMap out(values + start, len);
        const std::complex<T>* n = scalarNumerator ? numerators : numerators + start;
        const std::complex<T>* z = scalarDenominator ? denominators : denominators + start;
        if (scalarNumerator) {
            out = n[0] / ConstArrayMap(z, len);
        } else if (scalarDenominator) {
            out = ConstArrayMap(n, len) / z[0];
        } else {
            out = ConstArrayMap(n, len) / ConstArrayMap(z, len);
        }
        // vectorizable scan first: x - x is nonzero only for Inf or NaN
        bool anySpecial = false;
        indexType zLen = scalarDenominator ? 1 : len;
        for (indexType k = 0; k < zLen; ++k) {
            T zSum = z[k].real() + z[k].imag();
            anySpecial |= ((zSum - zSum) != 0) | (z[k].real() * z[k].imag() == 0);
        }
        indexType nLen = scalarNumerator ? 1 : len;
        for (indexType k = 0; k < nLen; ++k) {
            T nSum = n[k].real() + n[k].imag();
            anySpecial |= (nSum - nSum) != 0;
        }
        if (!anySpecial) {
            continue;
        }
        for (indexType k = 0; k < len; ++k) {
            const std::complex<T> nk = n[scalarNumerator ? 0 : k];
            const std::complex<T> zk = z[scalarDenominator ? 0 : k];
            if (complexQuotientIsSpecial<T>(nk, zk)) {
                values[start + k] = complexQuotientSpecial<T>(nk, zk);
            }
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
    blockedComplexQuotient<T>(Cz, Az, true, Bz, false, Clen);
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
    blockedComplexQuotient<T>(Cz, Az, false, Bz, true, Clen);
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
    blockedComplexQuotient<T>(Cz, Az, false, Bz, false, Clen);
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
            Cz[m] = complexQuotient<T>(Az[i], Bz[j]);
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
            Cz[m] = complexQuotient<T>(Az[j], Bz[i]);
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
            Cz[m] = complexQuotient<T>(Az[q], Bz[m]);
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
            Cz[m] = complexQuotient<T>(Az[m], Bz[q]);
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
            Cz[m] = complexQuotient<T>(Az[j], Bz[m]);
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
            Cz[m] = complexQuotient<T>(Az[m], Bz[j]);
        }
    }
    return res;
}
//=============================================================================
// Complex by real divisor: (a + bi) / c = (a / c) + (b / c)i keeps Inf and NaN parts. Zero or
// non-finite divisors keep the generic path.
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
    const T* Br = reinterpret_cast<const T*>(B.dataPointer());
    indexType divisorCount = B.isScalar() ? 1 : B.dimensions().elementCount();
    if (!realFactorsAreFiniteNonZero<T>(Br, divisorCount)) {
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
                [](std::complex<T> a, std::complex<T> b) { return complexQuotient<T>(a, b); });
        });
}
//=============================================================================
} // namespace Nelson
//=============================================================================
