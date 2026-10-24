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
#include <optional>
#include <cmath>
#include "nlsBuildConfig.h"
#include <Eigen/Sparse>
#include "IsEqual.hpp"
#include "OverloadHelpers.hpp"
#include "FindCommonType.hpp"
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
        = (Eigen::SparseMatrix<std::complex<double>, 0, signedIndexType>*)A.sparseDataPointer();
    Eigen::SparseMatrix<std::complex<double>, 0, signedIndexType>* spMatB
        = (Eigen::SparseMatrix<std::complex<double>, 0, signedIndexType>*)B.sparseDataPointer();
    if ((spMatA == nullptr && spMatB) || (spMatB == nullptr && spMatA)) {
        return false;
    }
    if (spMatA == nullptr && spMatB == nullptr) {
        return A.nonzeros() == B.nonzeros();
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
        = (Eigen::SparseMatrix<double, 0, signedIndexType>*)A.sparseDataPointer();
    Eigen::SparseMatrix<double, 0, signedIndexType>* spMatB
        = (Eigen::SparseMatrix<double, 0, signedIndexType>*)B.sparseDataPointer();

    if ((spMatA == nullptr && spMatB) || (spMatB == nullptr && spMatA)) {
        return false;
    }
    if (spMatA == nullptr && spMatB == nullptr) {
        return A.nonzeros() == B.nonzeros();
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
        = (Eigen::SparseMatrix<logical, 0, signedIndexType>*)A.sparseDataPointer();
    Eigen::SparseMatrix<logical, 0, signedIndexType>* spMatB
        = (Eigen::SparseMatrix<logical, 0, signedIndexType>*)B.sparseDataPointer();
    if ((spMatA == nullptr && spMatB) || (spMatB == nullptr && spMatA)) {
        return false;
    }
    if (spMatA == nullptr && spMatB == nullptr) {
        return A.nonzeros() == B.nonzeros();
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
    auto* elementA = (ArrayOf*)A.dataPointer();
    auto* elementB = (ArrayOf*)B.dataPointer();
    for (ompIndexType k = 0; k < nbElements; k++) {
        ArrayOf el1 = elementA[k];
        ArrayOf el2 = elementB[k];
        // a missing string compares like NaN: never equal (see isequaln)
        if (!el1.isCharacterArray() || !el2.isCharacterArray()) {
            return false;
        }
        std::wstring strA = el1.asWideString();
        std::wstring strB = el2.asWideString();
        if (strA != strB) {
            return false;
        }
    }
    return true;
}
//=============================================================================
static bool
isEqualElement(Evaluator* eval, const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    // Nested elements (cell/struct contents) may differ in size or class from
    // each other, so resolve their common type and re-check dimensions through
    // the full comparison path instead of the raw same-type/same-size dispatch.
    ArrayOfVector args;
    args << A;
    args << B;
    NelsonType commonType = NLS_UNKNOWN;
    bool isSparse = false;
    bool isComplex = false;
    std::string commonTypeName;
    FindCommonType(args, commonType, isSparse, isComplex, commonTypeName);
    if (commonType == NLS_UNKNOWN) {
        return false;
    }
    return IsEqual(eval, args, commonType, isSparse, isComplex, needToOverload);
}
//=============================================================================
static bool
cell_IsEqual(Evaluator* eval, const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    if (A.elementCount() != B.elementCount()) {
        return false;
    }
    ArrayOf* elementsA = (ArrayOf*)A.dataPointer();
    ArrayOf* elementsB = (ArrayOf*)B.dataPointer();
    for (size_t k = 0; k < A.elementCount(); ++k) {
        bool res = isEqualElement(eval, elementsA[k], elementsB[k], needToOverload);
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
    if (A.isEmpty()) {
        // empty structure arrays of the same size are equal when they have the
        // same number of fields, whatever their names
        return A.fieldNames().size() == B.fieldNames().size();
    }
    stringVector fieldnamesA = A.fieldNames();
    stringVector fieldnamesB = B.fieldNames();

    std::sort(fieldnamesA.begin(), fieldnamesA.end());
    std::sort(fieldnamesB.begin(), fieldnamesB.end());

    if (fieldnamesA.size() != fieldnamesB.size()
        || !std::equal(
            fieldnamesA.begin(), fieldnamesA.end(), fieldnamesB.begin(), fieldnamesB.end())) {
        return false;
    }
    for (auto& name : fieldnamesA) {
        ArrayOfVector fieldsA = A.fieldAsList(name);
        ArrayOfVector fieldsB = B.fieldAsList(name);
        if (fieldsA.size() != fieldsB.size()) {
            return false;
        }
        for (size_t k = 0; k < fieldsA.size(); ++k) {
            bool res = isEqualElement(eval, fieldsA[k], fieldsB[k], needToOverload);
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
    if (!A.dimensions().equals(B.dimensions())) {
        return false;
    }
    switch (A.dataClass()) {
    case NLS_DOUBLE: {
        if (A.isSparse()) {
            return sparsereal_IsEqual(A, B);
        }
        return real_IsEqual<double>(
            (const double*)A.dataPointer(), (const double*)B.dataPointer(), A.elementCount());
    } break;
    case NLS_SINGLE: {
        return real_IsEqual<single>(
            (const single*)A.dataPointer(), (const single*)B.dataPointer(), A.elementCount());
    } break;
    case NLS_DCOMPLEX: {
        if (A.isSparse()) {
            return sparsecomplex_IsEqual(A, B);
        }
        return complex_IsEqual<double>(
            (const double*)A.dataPointer(), (const double*)B.dataPointer(), A.elementCount());
    } break;
    case NLS_SCOMPLEX: {
        return complex_IsEqual<single>(
            (const single*)A.dataPointer(), (const single*)B.dataPointer(), A.elementCount());
    } break;
    case NLS_INT8: {
        return integer_IsEqual<int8>(
            (const int8*)A.dataPointer(), (const int8*)B.dataPointer(), (indexType)A.byteSize());
    } break;
    case NLS_INT16: {
        return integer_IsEqual<int16>(
            (const int16*)A.dataPointer(), (const int16*)B.dataPointer(), (indexType)A.byteSize());
    } break;
    case NLS_INT32: {
        return integer_IsEqual<int32>(
            (const int32*)A.dataPointer(), (const int32*)B.dataPointer(), (indexType)A.byteSize());
    } break;
    case NLS_INT64: {
        return integer_IsEqual<int64>(
            (const int64*)A.dataPointer(), (const int64*)B.dataPointer(), (indexType)A.byteSize());
    } break;
    case NLS_UINT8: {
        return integer_IsEqual<uint8>(
            (const uint8*)A.dataPointer(), (const uint8*)B.dataPointer(), (indexType)A.byteSize());
    } break;
    case NLS_UINT16: {
        return integer_IsEqual<uint16>((const uint16*)A.dataPointer(),
            (const uint16*)B.dataPointer(), (indexType)A.byteSize());
    } break;
    case NLS_UINT32: {
        return integer_IsEqual<uint32>((const uint32*)A.dataPointer(),
            (const uint32*)B.dataPointer(), (indexType)A.byteSize());
    } break;
    case NLS_UINT64: {
        return integer_IsEqual<uint64>((const uint64*)A.dataPointer(),
            (const uint64*)B.dataPointer(), (indexType)A.byteSize());
    } break;
    case NLS_LOGICAL: {
        if (A.isSparse()) {
            return sparselogical_IsEqual(A, B);
        }
        return integer_IsEqual<logical>((const logical*)A.dataPointer(),
            (const logical*)B.dataPointer(), (indexType)A.byteSize());
    } break;
    case NLS_CHAR: {
        return integer_IsEqual<charType>((const charType*)A.dataPointer(),
            (const charType*)B.dataPointer(), (indexType)A.byteSize());
    } break;
    case NLS_STRING_ARRAY: {
        return string_IsEqual(A, B, A.elementCount());
    } break;
    case NLS_STRUCT_ARRAY: {
        return struct_IsEqual(eval, A, B, needToOverload);
    } break;
    case NLS_CELL_ARRAY: {
        return cell_IsEqual(eval, A, B, needToOverload);
    } break;
    case NLS_MISSING_ARRAY: {
        // missing compares like NaN: never equal (see isequaln)
        return false;
    } break;
    case NLS_CLASS_ARRAY:
    case NLS_CLASSDEF_ARRAY: {
        bool overloadWasFound = false;
        ArrayOfVector args;
        args << A;
        args << B;
        std::string commonTypeName = ClassName(A);
        ArrayOf res = callOverloadedFunction(eval, NLS_OVERLOAD_ALL_TYPES, args, "isequal",
            commonTypeName, A.dataClass(), overloadWasFound);
        if (overloadWasFound) {
            return res.asLogicalScalar();
        }
        if (A.classType() != B.classType()) {
            return false;
        }
        return struct_IsEqual(eval, A, B, needToOverload);
    } break;
    case NLS_FUNCTION_HANDLE:
    case NLS_HANDLE:
    case NLS_GO_HANDLE: {
        bool overloadWasFound = false;
        ArrayOfVector args;
        args << A;
        args << B;
        std::string commonTypeName = ClassName(A);
        ArrayOf res = callOverloadedFunction(eval, NLS_OVERLOAD_ALL_TYPES, args, "isequal",
            commonTypeName, A.dataClass(), overloadWasFound);
        if (overloadWasFound) {
            return res.asLogicalScalar();
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
// For a 64-bit integer comparison class, a floating-point argument that is
// not integer-valued can never be equal: converting it would round it.
static bool
hasNonIntegerValuedFloatingArgument(const ArrayOfVector& args)
{
    for (const ArrayOf& value : args) {
        if (value.isSparse()) {
            continue;
        }
        if (value.dataClass() == NLS_DOUBLE) {
            const double* ptr = static_cast<const double*>(value.dataPointer());
            for (indexType k = 0; k < value.elementCount(); ++k) {
                if (std::floor(ptr[k]) != ptr[k]) {
                    return true;
                }
            }
        } else if (value.dataClass() == NLS_SINGLE) {
            const single* ptr = static_cast<const single*>(value.dataPointer());
            for (indexType k = 0; k < value.elementCount(); ++k) {
                if (std::floor(ptr[k]) != ptr[k]) {
                    return true;
                }
            }
        }
    }
    return false;
}
//=============================================================================
NelsonType
IsEqualComparisonType(const ArrayOfVector& args, NelsonType commonType)
{
    bool narrowClass = (commonType == NLS_SINGLE)
        || (IS_INTEGER_TYPE(commonType) && commonType != NLS_INT64 && commonType != NLS_UINT64);
    if (!narrowClass) {
        return commonType;
    }
    NelsonType firstClass = args[0].dataClass();
    for (size_t k = 1; k < args.size(); ++k) {
        if (args[k].dataClass() != firstClass) {
            // mixed classes: the values are compared exactly, in double
            // precision, instead of being rounded to the narrower class.
            return NLS_DOUBLE;
        }
    }
    return commonType;
}
//=============================================================================
bool
IsEqualConvertArgument(
    const ArrayOf& value, NelsonType comparisonType, bool isSparse, ArrayOf& converted)
{
    converted = value;
    switch (comparisonType) {
    case NLS_STRING_ARRAY: {
        // a string array compares with string arrays, character row vectors
        // and cell arrays of character vectors.
        if (value.isStringArray()) {
            return true;
        }
        if (value.isCharacterArray()) {
            if (!value.isRowVectorCharacterArray() && !value.isEmpty()) {
                return false;
            }
            converted = ArrayOf::stringArray(value.asWideString());
            return true;
        }
        if (value.isCellArrayOfCharacterVectors()) {
            bool needToOverload = false;
            converted = ArrayOf::toStringArray(value, needToOverload);
            return !needToOverload;
        }
        return false;
    } break;
    case NLS_CELL_ARRAY:
    case NLS_STRUCT_ARRAY: {
        return value.dataClass() == comparisonType;
    } break;
    default: {
        try {
            if (value.dataClass() != comparisonType) {
                converted.promoteType(comparisonType);
            }
            if (isSparse && !converted.isSparse()) {
                converted.makeSparse();
            }
        } catch (const NelsonException&) {
            return false;
        }
        return true;
    } break;
    }
}
//=============================================================================
bool
IsEqual(Evaluator* eval, const ArrayOfVector& args, NelsonType commonType, bool isSparse,
    bool isComplex, bool& needToOverload)
{
    needToOverload = false;
    NelsonType _commonType = IsEqualComparisonType(args, commonType);
    if (_commonType == NLS_DOUBLE && isComplex) {
        _commonType = NLS_DCOMPLEX;
    }
    if (_commonType == NLS_SINGLE && isComplex) {
        _commonType = NLS_SCOMPLEX;
    }
    if ((_commonType == NLS_INT64 || _commonType == NLS_UINT64)
        && hasNonIntegerValuedFloatingArgument(args)) {
        return false;
    }
    // conversions are rare: no ArrayOf (hence no allocation) is built otherwise
    const ArrayOf* firstElement = &args[0];
    std::optional<ArrayOf> firstConverted;
    if (IsEqualNeedsConversion(args[0], _commonType, isSparse)) {
        firstConverted.emplace();
        if (!IsEqualConvertArgument(args[0], _commonType, isSparse, *firstConverted)) {
            return false;
        }
        firstElement = &*firstConverted;
    }
    Dimensions commonDimensions = firstElement->dimensions();
    for (size_t k = 1; k < args.size(); ++k) {
        const ArrayOf* secondElement = &args[k];
        std::optional<ArrayOf> secondConverted;
        if (IsEqualNeedsConversion(args[k], _commonType, isSparse)) {
            secondConverted.emplace();
            if (!IsEqualConvertArgument(args[k], _commonType, isSparse, *secondConverted)) {
                return false;
            }
            secondElement = &*secondConverted;
        }
        if (!secondElement->dimensions().equals(commonDimensions)) {
            return false;
        }
        if (!isEqual(eval, *firstElement, *secondElement, needToOverload)) {
            return false;
        }
    }
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
