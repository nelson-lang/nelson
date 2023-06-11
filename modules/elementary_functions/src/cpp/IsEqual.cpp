//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <string.h>
#include <Eigen/Sparse>
#include "nlsBuildConfig.h"
#include "IsEqual.hpp"
#include "ImagPart.hpp"
#include "RealPart.hpp"
#include "Exception.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
bool
isequalornan(T a, T b)
{
    return (std::isnan(a) && std::isnan(b)) || (a == b);
}
//=============================================================================
template <class T>
bool
integer_IsEqual(T* ptrA, T* ptrB, indexType byteSize)
{
    return (memcmp(ptrA, ptrB, byteSize) == 0);
}
//=============================================================================
template <class T>
static bool
real_IsEqual(const ArrayOf& A, const ArrayOf& B, bool withNaN)
{
    ompIndexType nbElementsA = (ompIndexType)A.getElementCount();
    auto* ptrA = (T*)A.getDataPointer();
    auto* ptrB = (T*)B.getDataPointer();
    bool equal = true;
    if (withNaN) {
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for shared(equal)
#endif
        for (ompIndexType k = 0; k < nbElementsA; k++) {
            if (equal && !isequalornan(ptrA[k], ptrB[k])) {
#if defined(_NLS_WITH_OPENMP)
#pragma omp critical
#endif
                equal = false;
            }
        }
    } else {
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for shared(equal)
#endif
        for (ompIndexType k = 0; k < nbElementsA; k++) {
            if (equal && ptrA[k] != ptrB[k]) {
#if defined(_NLS_WITH_OPENMP)
#pragma omp critical
#endif
                equal = false;
            }
        }
    }
    return equal;
}
//=============================================================================
template <class T>
static bool
complex_IsEqual(ArrayOf& A, ArrayOf& B, bool withNaN)
{
    ompIndexType nbElementsA = (ompIndexType)A.getElementCount() * 2;
    auto* ptrA = (T*)A.getDataPointer();
    auto* ptrB = (T*)B.getDataPointer();
    bool equal = true;
    if (withNaN) {
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for shared(equal)
#endif
        for (ompIndexType k = 0; k < (ompIndexType)nbElementsA; k = k + 2) {
            if (equal
                && (!isequalornan<T>(ptrA[k], ptrB[k])
                    || !isequalornan<T>(ptrA[k + 1], ptrB[k + 1]))) {
#if defined(_NLS_WITH_OPENMP)
#pragma omp critical
#endif
                equal = false;
            }
        }
    } else {
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for shared(equal)
#endif
        for (ompIndexType k = 0; k < nbElementsA; k = k + 2) {
            if (equal && (ptrA[k] != ptrB[k]) || (ptrA[k + 1] != ptrB[k + 1])) {
#if defined(_NLS_WITH_OPENMP)
#pragma omp critical
#endif
                equal = false;
            }
        }
    }
    return equal;
}
//=============================================================================
static bool
string_IsEqual(ArrayOf& A, ArrayOf& B, bool sameTypes, bool withNaN)
{
    indexType nbElementsA = A.getElementCount();
    auto* elementA = (ArrayOf*)A.getDataPointer();
    auto* elementB = (ArrayOf*)B.getDataPointer();
    for (indexType k = 0; k < nbElementsA; k++) {
        ArrayOf el1 = elementA[k];
        ArrayOf el2 = elementB[k];
        bool isMissingEl1 = !el1.isCharacterArray();
        bool isMissingEl2 = !el2.isCharacterArray();
        if (isMissingEl1 && isMissingEl2) {
            return true;
        }
        if (isMissingEl1 || isMissingEl2) {
            return false;
        }
        if (!IsEqual(el1, el2, sameTypes, NLS_CHAR, withNaN)) {
            return false;
        }
    }
    return true;
}
//=============================================================================
template <class T>
static bool
haveSameIndexes(Eigen::SparseMatrix<T, 0, signedIndexType>* spMatA,
    Eigen::SparseMatrix<T, 0, signedIndexType>* spMatB)
{
    std::vector<signedIndexType> iA;
    std::vector<signedIndexType> jA;
    std::vector<signedIndexType> iB;
    std::vector<signedIndexType> jB;
    iA.reserve(spMatA->innerSize());
    jA.reserve(spMatA->outerSize());
    iB.reserve(spMatB->innerSize());
    jB.reserve(spMatB->outerSize());

    for (indexType k = 0; k < (indexType)spMatA->outerSize(); ++k) {
        for (typename Eigen::SparseMatrix<T, 0, signedIndexType>::InnerIterator it(*spMatA, k); it;
             ++it) {
            iA.push_back(it.row());
            jA.push_back(it.col());
        }
        for (typename Eigen::SparseMatrix<T, 0, signedIndexType>::InnerIterator it(*spMatB, k); it;
             ++it) {
            iB.push_back(it.row());
            jB.push_back(it.col());
        }
    }

    bool equal = memcmp(iA.data(), iB.data(), sizeof(signedIndexType) * iA.size()) == 0;
    if (!equal) {
        return equal;
    }
    return memcmp(jA.data(), jB.data(), sizeof(signedIndexType) * jA.size()) == 0;
}
//=============================================================================
template <class T>
static bool
sparsecomplex_IsEqual(ArrayOf& A, ArrayOf& B, bool withNaN)
{
    Eigen::SparseMatrix<std::complex<T>, 0, signedIndexType>* spMatA
        = (Eigen::SparseMatrix<std::complex<T>, 0, signedIndexType>*)A.getSparseDataPointer();
    Eigen::SparseMatrix<std::complex<T>, 0, signedIndexType>* spMatB
        = (Eigen::SparseMatrix<std::complex<T>, 0, signedIndexType>*)B.getSparseDataPointer();
    if ((spMatA == nullptr && spMatB) || (spMatB == nullptr && spMatA)) {
        return false;
    }
    if (spMatA == nullptr && spMatB == nullptr) {
        return A.getNonzeros() == B.getNonzeros();
    }
    const std::complex<T>* valuesA = spMatA->valuePtr();
    const std::complex<T>* valuesB = spMatB->valuePtr();
    if (spMatA->nonZeros() != spMatB->nonZeros()) {
        return false;
    }
    if (spMatA->outerSize() != spMatB->outerSize()) {
        return false;
    }
    bool equal = true;
    if (withNaN) {
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for shared(equal)
#endif
        for (ompIndexType k = 0; k < (ompIndexType)spMatA->nonZeros(); k++) {
            if (equal
                && (!isequalornan<T>(valuesA[k].real(), valuesB[k].real())
                    || !isequalornan<T>(valuesA[k].imag(), valuesB[k].imag()))) {
#if defined(_NLS_WITH_OPENMP)
#pragma omp critical
#endif
                equal = false;
            }
        }
    } else {
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for shared(equal)
#endif
        for (ompIndexType k = 0; k < spMatA->nonZeros(); k++) {
            if (equal && (valuesA[k].real() != valuesB[k].real())
                || (valuesA[k].imag() != valuesB[k].imag())) {
#if defined(_NLS_WITH_OPENMP)
#pragma omp critical
#endif
                equal = false;
            }
        }
    }
    if (!equal) {
        return equal;
    }
    return haveSameIndexes<std::complex<T>>(spMatA, spMatB);
}
//=============================================================================
template <class T>
static bool
sparsereal_IsEqual(ArrayOf& A, ArrayOf& B, bool withNaN)
{
    Eigen::SparseMatrix<T, 0, signedIndexType>* spMatA
        = (Eigen::SparseMatrix<T, 0, signedIndexType>*)A.getSparseDataPointer();
    Eigen::SparseMatrix<T, 0, signedIndexType>* spMatB
        = (Eigen::SparseMatrix<T, 0, signedIndexType>*)B.getSparseDataPointer();

    if ((spMatA == nullptr && spMatB) || (spMatB == nullptr && spMatA)) {
        return false;
    }
    if (spMatA == nullptr && spMatB == nullptr) {
        return A.getNonzeros() == B.getNonzeros();
    }
    const T* valuesA = spMatA->valuePtr();
    const T* valuesB = spMatB->valuePtr();

    if (spMatA->nonZeros() != spMatB->nonZeros()) {
        return false;
    }
    if (spMatA->outerSize() != spMatB->outerSize()) {
        return false;
    }
    bool equal = true;
    if (withNaN) {
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for shared(equal)
#endif
        for (ompIndexType k = 0; k < (ompIndexType)spMatA->nonZeros(); k++) {
            if (equal && !isequalornan<T>(valuesA[k], valuesB[k])) {
#if defined(_NLS_WITH_OPENMP)
#pragma omp critical
#endif
                equal = false;
            }
        }
    } else {
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for shared(equal)
#endif
        for (ompIndexType k = 0; k < (ompIndexType)spMatA->nonZeros(); k++) {
#if defined(_NLS_WITH_OPENMP)
#pragma omp critical
#endif
            if (equal && valuesA[k] != valuesB[k]) {
                equal = false;
            }
        }
    }
    if (!equal) {
        return equal;
    }
    return haveSameIndexes<T>(spMatA, spMatB);
}
//=============================================================================
template <class T>
static bool
sparselogical_IsEqual(ArrayOf& A, ArrayOf& B)
{
    Eigen::SparseMatrix<T, 0, signedIndexType>* spMatA
        = (Eigen::SparseMatrix<T, 0, signedIndexType>*)A.getSparseDataPointer();
    Eigen::SparseMatrix<T, 0, signedIndexType>* spMatB
        = (Eigen::SparseMatrix<T, 0, signedIndexType>*)B.getSparseDataPointer();
    if ((spMatA == nullptr && spMatB) || (spMatB == nullptr && spMatA)) {
        return false;
    }
    if (spMatA == nullptr && spMatB == nullptr) {
        return A.getNonzeros() == B.getNonzeros();
    }
    const T* valuesA = spMatA->valuePtr();
    const T* valuesB = spMatB->valuePtr();
    if ((spMatA->nonZeros() != spMatB->nonZeros())) {
        return false;
    }
    if (spMatA->innerSize() != spMatB->innerSize()) {
        return false;
    }
    return haveSameIndexes<T>(spMatA, spMatB);
}
//=============================================================================
bool
IsEqual(ArrayOf& A, ArrayOf& B, bool sameTypes, NelsonType commonType, bool withNaN)
{
    Dimensions dimsA = A.getDimensions();
    Dimensions dimsB = B.getDimensions();
    if (!dimsA.equals(dimsB)) {
        return false;
    }
    if (sameTypes) {
        if (A.getDataClass() != B.getDataClass()) {
            return false;
        }
    }
    if ((A.name() == B.name()) && (A.name() != "")) {
        return true;
    }
    if (A.isSparse() || B.isSparse()) {
        try {
            if (!A.isSparse()) {
                A.makeSparse();
            }
            if (!B.isSparse()) {
                B.makeSparse();
            }

            NelsonType destinationType;
            if (A.getDataClass() != B.getDataClass()) {
                destinationType = A.isComplex() || B.isComplex() ? NLS_DCOMPLEX : NLS_DOUBLE;
                A.promoteType(destinationType);
                B.promoteType(destinationType);
            } else {
                if (A.isEmpty() && B.isEmpty()) {
                    return A.getDimensions().equals(B.getDimensions());
                }
            }
            switch (A.getDataClass()) {
            case NLS_DOUBLE: {
                return sparsereal_IsEqual<double>(A, B, withNaN);
            } break;
            case NLS_LOGICAL: {
                return sparselogical_IsEqual<logical>(A, B);
            } break;
            case NLS_DCOMPLEX: {
                return sparsecomplex_IsEqual<double>(A, B, withNaN);
            } break;
            default: {
                return false;
            }
            }
            return false;
        } catch (const Exception&) {
            return false;
        }
    }

    if (A.getDataPointer() == B.getDataPointer()) {
        return true;
    }

    indexType nbElementsA = dimsA.getElementCount();
    if (A.isStringArray() || B.isStringArray()) {
        if (A.isStringArray() && B.isStringArray()) {
            return string_IsEqual(A, B, sameTypes, withNaN);
        }
        return false;
    }

    if (A.getDataClass() == B.getDataClass()) {
        switch (A.getDataClass()) {
        case NLS_DOUBLE: {
            return real_IsEqual<double>(A, B, withNaN);
        } break;
        case NLS_SINGLE: {
            return real_IsEqual<single>(A, B, withNaN);
        } break;
        case NLS_DCOMPLEX: {
            return complex_IsEqual<double>(A, B, withNaN);
        } break;
        case NLS_SCOMPLEX: {
            return complex_IsEqual<single>(A, B, withNaN);
        } break;
        case NLS_INT8: {
            return integer_IsEqual<int8>(
                (int8*)A.getDataPointer(), (int8*)B.getDataPointer(), (indexType)A.getByteSize());
        } break;
        case NLS_INT16: {
            return integer_IsEqual<int16>(
                (int16*)A.getDataPointer(), (int16*)B.getDataPointer(), (indexType)A.getByteSize());
        } break;
        case NLS_INT32: {
            return integer_IsEqual<int32>(
                (int32*)A.getDataPointer(), (int32*)B.getDataPointer(), (indexType)A.getByteSize());
        } break;
        case NLS_INT64: {
            return integer_IsEqual<int64>(
                (int64*)A.getDataPointer(), (int64*)B.getDataPointer(), (indexType)A.getByteSize());
        } break;
        case NLS_UINT8: {
            return integer_IsEqual<uint8>(
                (uint8*)A.getDataPointer(), (uint8*)B.getDataPointer(), (indexType)A.getByteSize());
        } break;
        case NLS_UINT16: {
            return integer_IsEqual<uint16>((uint16*)A.getDataPointer(), (uint16*)B.getDataPointer(),
                (indexType)A.getByteSize());
        } break;
        case NLS_UINT32: {
            return integer_IsEqual<uint32>((uint32*)A.getDataPointer(), (uint32*)B.getDataPointer(),
                (indexType)A.getByteSize());
        } break;
        case NLS_UINT64: {
            return integer_IsEqual<uint64>((uint64*)A.getDataPointer(), (uint64*)B.getDataPointer(),
                (indexType)A.getByteSize());
        } break;
        case NLS_LOGICAL: {
            return integer_IsEqual<logical>((logical*)A.getDataPointer(),
                (logical*)B.getDataPointer(), (indexType)A.getByteSize());
        } break;
        case NLS_CHAR: {
            return integer_IsEqual<charType>((charType*)A.getDataPointer(),
                (charType*)B.getDataPointer(), (indexType)A.getByteSize());
        } break;
        }
    } else {
    }

    bool isComplexA = A.getDataClass() == NLS_DCOMPLEX || A.getDataClass() == NLS_SCOMPLEX;
    bool isComplexB = B.getDataClass() == NLS_DCOMPLEX || B.getDataClass() == NLS_SCOMPLEX;
    bool isRealA = A.getDataClass() == NLS_DOUBLE || A.getDataClass() == NLS_SINGLE;
    bool isRealB = B.getDataClass() == NLS_DOUBLE || B.getDataClass() == NLS_SINGLE;
    bool isSingleOrDoubleA = A.getDataClass() == NLS_SINGLE || A.getDataClass() == NLS_DOUBLE;
    bool isSingleOrDoubleB = B.getDataClass() == NLS_SINGLE || B.getDataClass() == NLS_DOUBLE;
    if ((isSingleOrDoubleA || isComplexA) && (isSingleOrDoubleB || isComplexB)) {
        if (isRealA && isRealB) {
            A.promoteType(NLS_DOUBLE);
            B.promoteType(NLS_DOUBLE);
            return real_IsEqual<double>(A, B, withNaN);
        }
        A.promoteType(NLS_DCOMPLEX);
        B.promoteType(NLS_DCOMPLEX);
        return complex_IsEqual<double>(A, B, withNaN);
    }
    try {
        A.promoteType(NLS_DOUBLE);
        B.promoteType(NLS_DOUBLE);
    } catch (const Exception&) {
        return false;
    }
    return real_IsEqual<double>(A, B, withNaN);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
