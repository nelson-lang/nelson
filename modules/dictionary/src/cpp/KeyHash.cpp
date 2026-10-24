//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/format.h>
#include <fmt/xchar.h>
#include <random>
#include <type_traits>
#include <Eigen/Sparse>
#include "KeyHash.hpp"
#include "AnonymousMacroFunctionDef.hpp"
#include "HandleGenericObject.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::size_t randomSeed;
//=============================================================================
static std::size_t
getRandomHashPart();
//=============================================================================
inline size_t
hash_combine(size_t seed, size_t value)
{
    seed ^= value + 0x9e3779b9 + (seed << 6) + (seed >> 2);
    return seed;
}
//=============================================================================
template <typename T>
uint64_t
hash_combine(uint64_t seed, const T* ptr, indexType nbElements)
{
    for (indexType k = 0; k < nbElements; ++k) {
        T value = ptr[k];
        if constexpr (std::is_floating_point_v<T>) {
            // normalize -0.0 to +0.0 so that 0 and -0 hash identically
            if (value == static_cast<T>(0)) {
                value = static_cast<T>(0);
            }
        }
        std::size_t hash = std::hash<T>()(value);
        seed = hash_combine(seed, hash);
    }
    return seed;
}
//=============================================================================
template <typename T>
void
hashData(size_t& hashValue, const T* dataPtr, size_t elementCount)
{
    hashValue = hash_combine<T>(hashValue, dataPtr, elementCount);
}
//=============================================================================
template <>
void
hashData<std::complex<double>>(
    size_t& hashValue, const std::complex<double>* dataPtr, size_t elementCount)
{
    double* ptr = (double*)(dataPtr);
    hashValue = hash_combine<double>(hashValue, ptr, elementCount * 2);
}
//=============================================================================
template <typename T>
void
handleData(Evaluator* eval, size_t& hashValue, const ArrayOf& A)
{
    const T* ptr = static_cast<const T*>(A.dataPointer());
    hashData(hashValue, ptr, A.elementCount());
}
//=============================================================================
size_t
KeyHash(Evaluator* eval, const ArrayOf& A)
{
    if (randomSeed == 0) {
        randomSeed = getRandomHashPart();
    }
    size_t hashValue = 0;
    hashValue = hash_combine(hashValue, randomSeed);

    auto hashClass = [&hashValue](const ArrayOf& A) {
        std::size_t hashClass = std::hash<int>()(static_cast<int>(A.dataClass()));
        hashValue = hash_combine(hashValue, hashClass);
    };

    auto hashSparse = [&hashValue](const ArrayOf& A) {
        std::size_t hashIsSparse = std::hash<bool>()(static_cast<bool>(A.isSparse()));
        hashValue = hash_combine(hashValue, hashIsSparse);
    };

    auto hashDimensions = [&hashValue](const ArrayOf& A) {
        Dimensions dims = A.dimensions();
        std::vector<indexType> vectorDims = dims.asVector();
        hashValue = hash_combine<indexType>(hashValue, vectorDims.data(), vectorDims.size());
    };

    auto hashFunctionHandle = [&hashValue](const ArrayOf& A) {
        function_handle fh = A.asFunctionHandle();
        AnonymousMacroFunctionDef* cp
            = reinterpret_cast<AnonymousMacroFunctionDef*>(fh.anonymousHandle);
        std::string asString = cp->toString();
        size_t value = std::hash<std::string>()(asString);
        hashValue = hash_combine(hashValue, value);
    };

    auto hashGraphicsObjectHandle = [&hashValue](const ArrayOf& A) {
        go_handle handle = A.asGraphicsObjectScalar();
        std::size_t hashGO = std::hash<go_handle>()(handle);
        hashValue = hash_combine(hashValue, hashGO);
    };

    auto hashCellOrStringArray = [&hashValue, eval](const ArrayOf& A) {
        ArrayOf* elements = (ArrayOf*)(A.dataPointer());
        for (indexType k = 0; k < A.elementCount(); ++k) {
            size_t value = KeyHash(eval, elements[k]);
            hashValue = hash_combine(hashValue, value);
        }
    };

    auto hashStructOrClassArray = [&hashValue, eval](const ArrayOf& A) {
        stringVector names = A.fieldNames();
        for (auto& name : names) {
            size_t hashName = std::hash<std::string>()(name);
            hashValue = hash_combine(hashValue, hashName);
            ArrayOfVector fields = A.fieldAsList(name);
            for (size_t k = 0; k < fields.size(); ++k) {
                size_t value = KeyHash(eval, fields[k]);
                hashValue = hash_combine(hashValue, value);
            }
        }
    };

    auto hashGenericHandle = [&hashValue](const ArrayOf& A) {
        HandleGenericObject* ptr = A.asHandleScalar();
        if (ptr) {
            hashValue = hash_combine(hashValue, ptr->keyHash());
        } else {
            std::string message
                = formatI18nMessage(resolveMessageTemplate("Nelson:dictionary:unhashableTypeS",
                                        "Unhashable type '%s'."),
                    A.handleCategory());
            Error("Nelson:dictionary:unhashableType", message);
        }
    };

    // Sparse storage only holds nonZeros() values: hash those plus their
    // positions (elementCount() would read past the end of valuePtr()).
    auto hashNumeric = [&hashValue](const ArrayOf& A) {
        if (A.isSparse()) {
            Eigen::SparseMatrix<double, 0, signedIndexType>* spMat
                = (Eigen::SparseMatrix<double, 0, signedIndexType>*)(A.sparseDataPointer());
            spMat->makeCompressed();
            auto nnz = static_cast<indexType>(spMat->nonZeros());
            hashValue = hash_combine<double>(hashValue, spMat->valuePtr(), nnz);
            hashValue = hash_combine<signedIndexType>(hashValue, spMat->innerIndexPtr(), nnz);
        } else {
            const double* ptr = static_cast<const double*>(A.dataPointer());
            hashValue = hash_combine<double>(hashValue, ptr, A.elementCount());
        }
    };

    auto hashLogical = [&hashValue](const ArrayOf& A) {
        if (A.isSparse()) {
            Eigen::SparseMatrix<logical, 0, signedIndexType>* spMat
                = (Eigen::SparseMatrix<logical, 0, signedIndexType>*)(A.sparseDataPointer());
            spMat->makeCompressed();
            auto nnz = static_cast<indexType>(spMat->nonZeros());
            hashValue = hash_combine<logical>(hashValue, spMat->valuePtr(), nnz);
            hashValue = hash_combine<signedIndexType>(hashValue, spMat->innerIndexPtr(), nnz);
        } else {
            handleData<logical>(nullptr, hashValue, A);
        }
    };

    auto hashComplex = [&hashValue](const ArrayOf& A) {
        if (A.isSparse()) {
            Eigen::SparseMatrix<std::complex<double>, 0, signedIndexType>* spMat
                = (Eigen::SparseMatrix<std::complex<double>, 0, signedIndexType>*)(A
                        .sparseDataPointer());
            spMat->makeCompressed();
            auto nnz = static_cast<indexType>(spMat->nonZeros());
            double* ptr = reinterpret_cast<double*>(spMat->valuePtr());
            hashValue = hash_combine<double>(hashValue, ptr, nnz * 2);
            hashValue = hash_combine<signedIndexType>(hashValue, spMat->innerIndexPtr(), nnz);
        } else {
            const double* ptr = static_cast<const double*>(A.dataPointer());
            hashValue = hash_combine<double>(hashValue, ptr, A.elementCount() * 2);
        }
    };

    hashClass(A);
    hashSparse(A);
    hashDimensions(A);

    switch (A.dataClass()) {
    case NLS_MISSING_ARRAY:
        handleData<double>(nullptr, hashValue, A);
        break;
    case NLS_LOGICAL:
        hashNumeric(A);
        break;
    case NLS_DOUBLE:
        hashNumeric(A);
        break;
    case NLS_DCOMPLEX:
        hashComplex(A);
        break;
    case NLS_CHAR:
        handleData<charType>(nullptr, hashValue, A);
        break;
    case NLS_SINGLE:
        handleData<single>(nullptr, hashValue, A);
        break;
    case NLS_SCOMPLEX: {
        const single* ptr = static_cast<const single*>(A.dataPointer());
        hashValue = hash_combine<single>(hashValue, ptr, A.elementCount() * 2);
    } break;
    case NLS_INT8:
        handleData<int8>(nullptr, hashValue, A);
        break;
    case NLS_INT16:
        handleData<int16>(nullptr, hashValue, A);
        break;
    case NLS_INT32:
        handleData<int32>(nullptr, hashValue, A);
        break;
    case NLS_INT64:
        handleData<int64>(nullptr, hashValue, A);
        break;
    case NLS_UINT8:
        handleData<uint8>(nullptr, hashValue, A);
        break;
    case NLS_UINT16:
        handleData<uint16>(nullptr, hashValue, A);
        break;
    case NLS_UINT32:
        handleData<uint32>(nullptr, hashValue, A);
        break;
    case NLS_UINT64:
        handleData<uint64>(nullptr, hashValue, A);
        break;
    case NLS_FUNCTION_HANDLE:
        hashFunctionHandle(A);
        break;
    case NLS_GO_HANDLE:
        hashGraphicsObjectHandle(A);
        break;
    case NLS_CELL_ARRAY:
    case NLS_STRING_ARRAY:
        hashCellOrStringArray(A);
        break;
    case NLS_STRUCT_ARRAY:
    case NLS_CLASS_ARRAY:
    case NLS_CLASSDEF_ARRAY:
        hashStructOrClassArray(A);
        break;
    case NLS_HANDLE:
        hashGenericHandle(A);
        break;
    case NLS_UNKNOWN:
    default:
        Error(L"Nelson:containersMap:unhashableType", L"Unhashable type.");
        break;
    }
    return hashValue;
}
//=============================================================================
std::size_t
getRandomHashPart()
{
    std::random_device rd;
    std::default_random_engine rng(rd());
    std::uniform_int_distribution<std::size_t> dist(0, std::numeric_limits<std::size_t>::max());
    return dist(rng);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
