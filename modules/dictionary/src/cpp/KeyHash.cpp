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
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include <random>
#include <Eigen/Sparse>
#include "KeyHash.hpp"
#include "AnonymousMacroFunctionDef.hpp"
#include "HandleGenericObject.hpp"
#include "Error.hpp"
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
        std::size_t hash = std::hash<T>()(ptr[k]);
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
    const T* ptr = static_cast<const T*>(A.getDataPointer());
    hashData(hashValue, ptr, A.getElementCount());
}
//=============================================================================
/**
 * @brief Calculate the hash value for an ArrayOf object.
 *
 * @param eval Pointer to the Evaluator object.
 * @param A Reference to the ArrayOf object.
 * @return The calculated hash value.
 */
size_t
KeyHash(Evaluator* eval, const ArrayOf& A)
{
    // If randomSeed is not initialized, generate a random seed
    if (randomSeed == 0) {
        randomSeed = getRandomHashPart();
    }
    // Initialize hashValue
    size_t hashValue = 0;
    // Combine hashValue with randomSeed
    hashValue = hash_combine(hashValue, randomSeed);

    // Lambda function to hash the data class of the ArrayOf object
    auto hashClass = [&hashValue](const ArrayOf& A) {
        std::size_t hashClass = std::hash<int>()(static_cast<int>(A.getDataClass()));
        hashValue = hash_combine(hashValue, hashClass);
    };

    // Lambda function to hash whether the ArrayOf object is sparse
    auto hashSparse = [&hashValue](const ArrayOf& A) {
        std::size_t hashIsSparse = std::hash<bool>()(static_cast<bool>(A.isSparse()));
        hashValue = hash_combine(hashValue, hashIsSparse);
    };

    // Lambda function to hash the dimensions of the ArrayOf object
    auto hashDimensions = [&hashValue](const ArrayOf& A) {
        Dimensions dims = A.getDimensions();
        std::vector<indexType> vectorDims = dims.getAsVector();
        hashValue = hash_combine<indexType>(hashValue, vectorDims.data(), vectorDims.size());
    };

    // Lambda function to hash the function handle content of the ArrayOf object
    auto hashFunctionHandle = [&hashValue](const ArrayOf& A) {
        function_handle fh = A.getContentAsFunctionHandle();
        AnonymousMacroFunctionDef* cp
            = reinterpret_cast<AnonymousMacroFunctionDef*>(fh.anonymousHandle);
        std::string asString = cp->toString();
        size_t value = std::hash<std::string>()(asString);
        hashValue = hash_combine(hashValue, value);
    };

    // Lambda function to hash the graphics object handle content of the ArrayOf object
    auto hashGraphicsObjectHandle = [&hashValue](const ArrayOf& A) {
        go_handle handle = A.getContentAsGraphicsObjectScalar();
        std::size_t hashGO = std::hash<go_handle>()(handle);
        hashValue = hash_combine(hashValue, hashGO);
    };

    // Lambda function to hash cell or string array content of the ArrayOf object
    auto hashCellOrStringArray = [&hashValue, eval](const ArrayOf& A) {
        ArrayOf* elements = (ArrayOf*)(A.getDataPointer());
        for (indexType k = 0; k < A.getElementCount(); ++k) {
            size_t value = KeyHash(eval, elements[k]);
            hashValue = hash_combine(hashValue, value);
        }
    };

    // Lambda function to hash struct or class array content of the ArrayOf object
    auto hashStructOrClassArray = [&hashValue, eval](const ArrayOf& A) {
        stringVector names = A.getFieldNames();
        for (auto& name : names) {
            size_t hashName = std::hash<std::string>()(name);
            hashValue = hash_combine(hashValue, hashName);
            ArrayOfVector fields = A.getFieldAsList(name);
            for (size_t k = 0; k < fields.size(); ++k) {
                size_t value = KeyHash(eval, fields[k]);
                hashValue = hash_combine(hashValue, value);
            }
        }
    };

    // Lambda function to hash generic handle content of the ArrayOf object
    auto hashGenericHandle = [&hashValue](const ArrayOf& A) {
        HandleGenericObject* ptr = A.getContentAsHandleScalar();
        if (ptr) {
            hashValue = hash_combine(hashValue, ptr->keyHash());
        } else {
            std::string message = fmt::sprintf(_("Unhashable type '%s'."), A.getHandleCategory());
            Error(message);
        }
    };

    // Lambda function to hash numeric content of the ArrayOf object
    auto hashNumeric = [&hashValue](const ArrayOf& A) {
        if (A.isSparse()) {
            Eigen::SparseMatrix<double, 0, signedIndexType>* spMat
                = (Eigen::SparseMatrix<double, 0, signedIndexType>*)(A.getSparseDataPointer());
            double* ptr = spMat->valuePtr();
            hashValue = hash_combine<double>(hashValue, ptr, A.getElementCount());
        } else {
            const double* ptr = static_cast<const double*>(A.getDataPointer());
            hashValue = hash_combine<double>(hashValue, ptr, A.getElementCount());
        }
    };

    // Lambda function to hash complex content of the ArrayOf object
    auto hashComplex = [&hashValue](const ArrayOf& A) {
        if (A.isSparse()) {
            Eigen::SparseMatrix<std::complex<double>, 0, signedIndexType>* spMat
                = (Eigen::SparseMatrix<std::complex<double>, 0,
                    signedIndexType>*)(A.getSparseDataPointer());
            std::complex<double>* ptrZ = spMat->valuePtr();
            double* ptr = reinterpret_cast<double*>(ptrZ);
            hashValue = hash_combine<double>(hashValue, ptr, A.getElementCount() * 2);
        } else {
            const double* ptr = static_cast<const double*>(A.getDataPointer());
            hashValue = hash_combine<double>(hashValue, ptr, A.getElementCount() * 2);
        }
    };

    // Call lambda functions to hash different aspects of the ArrayOf object
    hashClass(A);
    hashSparse(A);
    hashDimensions(A);

    // Switch statement to handle hashing based on data class of the ArrayOf object
    switch (A.getDataClass()) {
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
    case NLS_SCOMPLEX:
        handleData<single>(nullptr, hashValue, A);
        break;
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
        hashStructOrClassArray(A);
        break;
    case NLS_HANDLE:
        hashGenericHandle(A);
        break;
    case NLS_UNKNOWN:
    default:
        Error(_W("Unhashable type."));
        break;
    }
    // Return the final hash value
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
}
//=============================================================================
