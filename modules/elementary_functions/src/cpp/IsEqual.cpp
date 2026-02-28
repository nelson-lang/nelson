//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <cstring>
#include "nlsBuildConfig.h"
#include <Eigen/Sparse>
#include "IsEqual.hpp"
#include "OverloadHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
isEqual(Evaluator* eval, const ArrayOf& A, const ArrayOf& B, bool& needToOverload);
//=============================================================================
template <class T>
static bool
integer_IsEqual(const T* ptrA, const T* ptrB, indexType byteSize)
{
    return (memcmp(ptrA, ptrB, byteSize) == 0);
}
//=============================================================================
template <class T>
static bool
real_IsEqual(const T* ptrA, const T* ptrB, ompIndexType nbElements)
{
    if (nbElements == 1) {
        return (ptrA[0] == ptrB[0]);
    }
#if WITH_OPENMP
    bool equal = true;
#if WITH_OPENMP
#pragma omp parallel for shared(equal) if (nbElements > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType k = 0; k < nbElements; k++) {
        if (equal && (ptrA[k] != ptrB[k])) {
#if WITH_OPENMP
#pragma omp critical
#endif
            equal = false;
        }
    }
    return equal;
#else
    for (ompIndexType k = 0; k < nbElements; ++k) {
        if (ptrA[k] != ptrB[k]) {
            return false;
        }
    }
    return true;
#endif
}
//=============================================================================
template <class T>
static bool
complex_IsEqual(const T* ptrA, const T* ptrB, ompIndexType nbElements)
{
    if (nbElements == 1) {
        return (ptrA[0] == ptrB[0]) && (ptrA[1] == ptrB[1]);
    }
    for (ompIndexType k = 0; k < nbElements * 2; ++k) {
        if (ptrA[k] != ptrB[k]) {
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
static bool
sparsecomplex_IsEqual(const ArrayOf& A, const ArrayOf& B)
{
    Eigen::SparseMatrix<std::complex<double>, 0, signedIndexType>* spMatA
        = (Eigen::SparseMatrix<std::complex<double>, 0, signedIndexType>*)A.getSparseDataPointer();
    Eigen::SparseMatrix<std::complex<double>, 0, signedIndexType>* spMatB
        = (Eigen::SparseMatrix<std::complex<double>, 0, signedIndexType>*)B.getSparseDataPointer();
    if ((spMatA == nullptr && spMatB) || (spMatB == nullptr && spMatA)) {
        return false;
    }
    if (spMatA == nullptr && spMatB == nullptr) {
        return A.getNonzeros() == B.getNonzeros();
    }
    if ((spMatA->nonZeros() != spMatB->nonZeros())) {
        return false;
    }
    if (spMatA->innerSize() != spMatB->innerSize()) {
        return false;
    }
    if ((spMatA->outerSize() == spMatB->outerSize()) && (spMatA->outerSize() == 0)) {
        return true;
    }
    if (spMatA == spMatB) {
        return true;
    }
    const std::complex<double>* valuesA = spMatA->valuePtr();
    const std::complex<double>* valuesB = spMatB->valuePtr();

#if WITH_OPENMP
    bool equal = true;
#pragma omp parallel for shared(equal)
    for (ompIndexType k = 0; k < (ompIndexType)spMatA->nonZeros(); k++) {
        if (equal
            && ((valuesA[k].real() != valuesB[k].real())
                || (valuesA[k].imag() != valuesB[k].imag())))
#pragma omp critical
            equal = false;
    }
    if (!equal) {
        return false;
    }
#else
    for (ompIndexType k = 0; k < (ompIndexType)spMatA->nonZeros(); k++) {
        if ((valuesA[k].real() != valuesB[k].real()) || (valuesA[k].imag() != valuesB[k].imag())) {
            return false;
        }
    }
#endif
    return haveSameIndexes<std::complex<double>>(spMatA, spMatB);
}
//=============================================================================
static bool
sparsereal_IsEqual(const ArrayOf& A, const ArrayOf& B)
{
    Eigen::SparseMatrix<double, 0, signedIndexType>* spMatA
        = (Eigen::SparseMatrix<double, 0, signedIndexType>*)A.getSparseDataPointer();
    Eigen::SparseMatrix<double, 0, signedIndexType>* spMatB
        = (Eigen::SparseMatrix<double, 0, signedIndexType>*)B.getSparseDataPointer();

    if ((spMatA == nullptr && spMatB) || (spMatB == nullptr && spMatA)) {
        return false;
    }
    if (spMatA == nullptr && spMatB == nullptr) {
        return A.getNonzeros() == B.getNonzeros();
    }
    if ((spMatA->nonZeros() != spMatB->nonZeros())) {
        return false;
    }
    if (spMatA->innerSize() != spMatB->innerSize()) {
        return false;
    }
    if ((spMatA->outerSize() == spMatB->outerSize()) && (spMatA->outerSize() == 0)) {
        return true;
    }
    if (spMatA == spMatB) {
        return true;
    }
    const double* valuesA = spMatA->valuePtr();
    const double* valuesB = spMatB->valuePtr();

#if WITH_OPENMP
    bool equal = true;
#pragma omp parallel for shared(equal)
    for (ompIndexType k = 0; k < (ompIndexType)spMatA->nonZeros(); k++) {
        if (equal && (valuesA[k] != valuesB[k])) {
#pragma omp critical
            equal = false;
        }
    }
    if (!equal) {
        return false;
    }
#else
    for (ompIndexType k = 0; k < (ompIndexType)spMatA->nonZeros(); k++) {
        if (valuesA[k] != valuesB[k]) {
            return false;
        }
    }
#endif
    return haveSameIndexes<double>(spMatA, spMatB);
}
//=============================================================================
static bool
sparselogical_IsEqual(const ArrayOf& A, const ArrayOf& B)
{
    Eigen::SparseMatrix<logical, 0, signedIndexType>* spMatA
        = (Eigen::SparseMatrix<logical, 0, signedIndexType>*)A.getSparseDataPointer();
    Eigen::SparseMatrix<logical, 0, signedIndexType>* spMatB
        = (Eigen::SparseMatrix<logical, 0, signedIndexType>*)B.getSparseDataPointer();
    if ((spMatA == nullptr && spMatB) || (spMatB == nullptr && spMatA)) {
        return false;
    }
    if (spMatA == nullptr && spMatB == nullptr) {
        return A.getNonzeros() == B.getNonzeros();
    }
    if ((spMatA->nonZeros() != spMatB->nonZeros())) {
        return false;
    }
    if (spMatA->innerSize() != spMatB->innerSize()) {
        return false;
    }
    if ((spMatA->outerSize() == spMatB->outerSize()) && (spMatA->outerSize() == 0)) {
        return true;
    }
    return haveSameIndexes<logical>(spMatA, spMatB);
}
//=============================================================================
static bool
string_IsEqual(const ArrayOf& A, const ArrayOf& B, ompIndexType nbElements)
{
    auto* elementA = (ArrayOf*)A.getDataPointer();
    auto* elementB = (ArrayOf*)B.getDataPointer();
    for (ompIndexType k = 0; k < nbElements; k++) {
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
        std::wstring strA = el1.getContentAsWideString();
        std::wstring strB = el2.getContentAsWideString();
        if (strA != strB) {
            return false;
        }
    }
    return true;
}
//=============================================================================
static bool
cell_IsEqual(Evaluator* eval, const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    ArrayOf* elementsA = (ArrayOf*)A.getDataPointer();
    ArrayOf* elementsB = (ArrayOf*)B.getDataPointer();
    for (size_t k = 0; k < A.getElementCount(); ++k) {
        bool res = isEqual(eval, elementsA[k], elementsB[k], needToOverload);
        if (!res) {
            return false;
        }
    }
    return true;
}
//=============================================================================
static bool
struct_IsEqual(Evaluator* eval, const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    stringVector fieldnamesA = A.getFieldNames();
    stringVector fieldnamesB = B.getFieldNames();

    std::sort(fieldnamesA.begin(), fieldnamesA.end());
    std::sort(fieldnamesB.begin(), fieldnamesB.end());

    if (!std::equal(
            fieldnamesA.begin(), fieldnamesA.end(), fieldnamesB.begin(), fieldnamesB.end())) {
        return false;
    }
    for (auto& name : fieldnamesA) {
        ArrayOfVector fieldsA = A.getFieldAsList(name);
        ArrayOfVector fieldsB = B.getFieldAsList(name);
        for (size_t k = 0; k < fieldsA.size(); ++k) {
            bool res = isEqual(eval, fieldsA[k], fieldsB[k], needToOverload);
            if (!res) {
                return false;
            }
        }
    }
    return true;
}
//=============================================================================
bool
isEqual(Evaluator* eval, const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    needToOverload = false;
    switch (A.getDataClass()) {
    case NLS_DOUBLE: {
        if (A.isSparse()) {
            return sparsereal_IsEqual(A, B);
        }
        return real_IsEqual<double>((const double*)A.getDataPointer(),
            (const double*)B.getDataPointer(), A.getElementCount());
    } break;
    case NLS_SINGLE: {
        return real_IsEqual<single>((const single*)A.getDataPointer(),
            (const single*)B.getDataPointer(), A.getElementCount());
    } break;
    case NLS_DCOMPLEX: {
        if (A.isSparse()) {
            return sparsecomplex_IsEqual(A, B);
        }
        return complex_IsEqual<double>((const double*)A.getDataPointer(),
            (const double*)B.getDataPointer(), A.getElementCount());
    } break;
    case NLS_SCOMPLEX: {
        return complex_IsEqual<single>((const single*)A.getDataPointer(),
            (const single*)B.getDataPointer(), A.getElementCount());
    } break;
    case NLS_INT8: {
        return integer_IsEqual<int8>((const int8*)A.getDataPointer(),
            (const int8*)B.getDataPointer(), (indexType)A.getByteSize());
    } break;
    case NLS_INT16: {
        return integer_IsEqual<int16>((const int16*)A.getDataPointer(),
            (const int16*)B.getDataPointer(), (indexType)A.getByteSize());
    } break;
    case NLS_INT32: {
        return integer_IsEqual<int32>((const int32*)A.getDataPointer(),
            (const int32*)B.getDataPointer(), (indexType)A.getByteSize());
    } break;
    case NLS_INT64: {
        return integer_IsEqual<int64>((const int64*)A.getDataPointer(),
            (const int64*)B.getDataPointer(), (indexType)A.getByteSize());
    } break;
    case NLS_UINT8: {
        return integer_IsEqual<uint8>((const uint8*)A.getDataPointer(),
            (const uint8*)B.getDataPointer(), (indexType)A.getByteSize());
    } break;
    case NLS_UINT16: {
        return integer_IsEqual<uint16>((const uint16*)A.getDataPointer(),
            (const uint16*)B.getDataPointer(), (indexType)A.getByteSize());
    } break;
    case NLS_UINT32: {
        return integer_IsEqual<uint32>((const uint32*)A.getDataPointer(),
            (const uint32*)B.getDataPointer(), (indexType)A.getByteSize());
    } break;
    case NLS_UINT64: {
        return integer_IsEqual<uint64>((const uint64*)A.getDataPointer(),
            (const uint64*)B.getDataPointer(), (indexType)A.getByteSize());
    } break;
    case NLS_LOGICAL: {
        if (A.isSparse()) {
            return sparselogical_IsEqual(A, B);
        }
        return integer_IsEqual<logical>((const logical*)A.getDataPointer(),
            (const logical*)B.getDataPointer(), (indexType)A.getByteSize());
    } break;
    case NLS_CHAR: {
        return integer_IsEqual<charType>((const charType*)A.getDataPointer(),
            (const charType*)B.getDataPointer(), (indexType)A.getByteSize());
    } break;
    case NLS_STRING_ARRAY: {
        return string_IsEqual(A, B, A.getElementCount());
    } break;
    case NLS_STRUCT_ARRAY: {
        return struct_IsEqual(eval, A, B, needToOverload);
    } break;
    case NLS_CELL_ARRAY: {
        return cell_IsEqual(eval, A, B, needToOverload);
    } break;
    case NLS_FUNCTION_HANDLE:
    case NLS_CLASS_ARRAY:
    case NLS_HANDLE:
    case NLS_GO_HANDLE: {
        bool overloadWasFound = false;
        ArrayOfVector args;
        args << A;
        args << B;
        std::string commonTypeName = ClassName(A);
        ArrayOf res = callOverloadedFunction(eval, NLS_OVERLOAD_ALL_TYPES, args, "isequaln",
            commonTypeName, A.getDataClass(), overloadWasFound);
        if (overloadWasFound) {
            return res.getContentAsLogicalScalar();
        }
        needToOverload = true;
    } break;
    default: {
        needToOverload = true;
    } break;
    }
    return false;
}
//=============================================================================
bool
IsEqual(Evaluator* eval, const ArrayOfVector& args, NelsonType commonType, bool isSparse,
    bool isComplex, bool& needToOverload)
{
    needToOverload = false;
    ArrayOf firstElement = args[0];
    NelsonType _commonType = commonType;
    if (commonType == NLS_DOUBLE && isComplex) {
        _commonType = NLS_DCOMPLEX;
    }
    if (commonType == NLS_SINGLE && isComplex) {
        _commonType = NLS_SCOMPLEX;
    }
    try {
        if (_commonType == NLS_STRING_ARRAY && (firstElement.getDataClass() == NLS_CHAR)) {
            firstElement = ArrayOf::stringArrayConstructor(firstElement.getContentAsWideString());
        } else {
            if (firstElement.getDataClass() != _commonType) {
                firstElement.promoteType(_commonType);
            }
            if (!firstElement.isSparse() && isSparse) {
                firstElement.makeSparse();
            }
        }

    } catch (const Exception&) {
        return false;
    }
    Dimensions commonDimensions = firstElement.getDimensions();
    for (auto k = 1; k < args.size(); ++k) {
        ArrayOf secondElement = args[k];
        try {
            if (_commonType == NLS_STRING_ARRAY && (secondElement.getDataClass() == NLS_CHAR)) {
                secondElement
                    = ArrayOf::stringArrayConstructor(secondElement.getContentAsWideString());
            } else {
                if (secondElement.getDataClass() != _commonType) {
                    secondElement.promoteType(_commonType);
                }
                if (!secondElement.isSparse() && isSparse) {
                    secondElement.makeSparse();
                }
            }
        } catch (const Exception&) {
            return false;
        }
        if (!secondElement.getDimensions().equals(commonDimensions)) {
            return false;
        }
        if (!isEqual(eval, firstElement, secondElement, needToOverload)) {
            return false;
        }
    }
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
