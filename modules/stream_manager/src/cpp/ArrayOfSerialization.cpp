//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define EIGEN_NO_DEBUG
//=============================================================================
#include <Eigen/Sparse>
#include "ArrayOfSerialization.hpp"
#include "Error.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
void
eigenSparseToIJV(const Eigen::SparseMatrix<T, 0, signedIndexType>& M, std::vector<uint64>& I,
    std::vector<uint64>& J, std::vector<T>& V, uint64& nzmax)
{
    I.reserve(M.nonZeros());
    J.reserve(M.nonZeros());
    V.reserve(M.nonZeros());
    nzmax = M.data().allocatedSize();
    for (int i = 0; i < M.outerSize(); i++) {
        for (typename Eigen::SparseMatrix<T, 0, signedIndexType>::InnerIterator it(M, i); it;
             ++it) {
            I.push_back(it.row());
            J.push_back(it.col());
            V.push_back(it.value());
        }
    }
}
//=============================================================================
template <class T>
void*
IJVToAllocatedEigenSparse(const std::vector<uint64>& I, const std::vector<uint64>& J,
    const std::vector<T>& V, uint64 cols, uint64 rows, uint64 nzmax)
{
    std::vector<Eigen::Triplet<T>> tripletList;
    size_t sizeI = I.size();
    tripletList.reserve(sizeI);
    if (sizeI == J.size() && J.size() == V.size() && sizeI != 0) {
        for (indexType k = 0; k < sizeI; ++k) {
            tripletList.push_back(Eigen::Triplet<T>(I[k], J[k], V[k]));
        }
        Eigen::SparseMatrix<T, 0, signedIndexType>* spMat;
        try {
            spMat = new Eigen::SparseMatrix<T, 0, signedIndexType>(cols, rows);

        } catch (const std::bad_alloc&) {
            spMat = nullptr;
            Error(ERROR_MEMORY_ALLOCATION);
        }
        if (spMat) {
            spMat->setFromTriplets(tripletList.begin(), tripletList.end());
            spMat->reserve(nzmax);
            spMat->finalize();
            spMat->makeCompressed();
            spMat->data().squeeze();
            return (void*)spMat;
        }
    }
    return nullptr;
}
//=============================================================================
ArrayOfSerialization::ArrayOfSerialization() { clear(); }
//=============================================================================
ArrayOfSerialization::ArrayOfSerialization(const ArrayOf& data)
{
    clear();
    fullySerialized = set(data);
}
//=============================================================================
bool
ArrayOfSerialization::isFullySerialized()
{
    return fullySerialized;
}
//=============================================================================
void
ArrayOfSerialization::clear()
{
    fullySerialized = false;
    nelsonObjectClass = (int)NLS_UNKNOWN;
    isSparse = false;
    dims.clear();
    fieldnames.clear();
    asInt8.clear();
    asUint8.clear();
    asInt16.clear();
    asUint16.clear();
    asInt32.clear();
    asUint32.clear();
    asInt64.clear();
    asUint64.clear();
    asDouble.clear();
    asSingle.clear();
    asCharacter.clear();
    nzmax = 0;
    I.clear();
    J.clear();
    subArrayOf.clear();
}
//=============================================================================
bool
ArrayOfSerialization::set(const ArrayOf& data)
{
    nelsonObjectClass = (int)data.getDataClass();
    isSparse = data.isSparse();
    Dimensions dimsData = data.getDimensions();
    dims = dimsData.getAsVector();
    switch (data.getDataClass()) {
    case NLS_CELL_ARRAY:
    case NLS_STRING_ARRAY:
    case NLS_CLASS_ARRAY:
    case NLS_FUNCTION_HANDLE:
    case NLS_STRUCT_ARRAY: {
        ArrayOf* elements = (ArrayOf*)data.getDataPointer();
        indexType nbElements = dimsData.getElementCount();
        fieldnames = data.getFieldNames();
        subArrayOf.reserve(nbElements);
        fullySerialized = true;
        for (indexType k = 0; k < nbElements; ++k) {
            ArrayOfSerialization object(elements[k]);
            subArrayOf.push_back(object);
            if (!object.isFullySerialized()) {
                fullySerialized = false;
            }
        }
    } break;
    case NLS_LOGICAL: {
        if (!isSparse) {
            logical* ptrLogical = (logical*)data.getDataPointer();
            asUint8.reserve(dimsData.getElementCount());
            asUint8.assign(ptrLogical, ptrLogical + dimsData.getElementCount());
        } else {
            const Eigen::SparseMatrix<uint8, 0, signedIndexType>* spMat
                = (Eigen::SparseMatrix<uint8, 0, signedIndexType>*)data.getSparseDataPointer();
            if (spMat) {
                eigenSparseToIJV<uint8>(*spMat, I, J, asUint8, nzmax);
            }
        }
        fullySerialized = true;
    } break;
    case NLS_UINT8: {
        uint8* ptrUint8 = (uint8*)data.getDataPointer();
        asUint8.reserve(dimsData.getElementCount());
        asUint8.assign(ptrUint8, ptrUint8 + dimsData.getElementCount());
        fullySerialized = true;
    } break;
    case NLS_INT8: {
        int8* ptrInt8 = (int8*)data.getDataPointer();
        asInt8.reserve(dimsData.getElementCount());
        asInt8.assign(ptrInt8, ptrInt8 + dimsData.getElementCount());
        fullySerialized = true;
    } break;
    case NLS_UINT16: {
        uint16* ptrUint16 = (uint16*)data.getDataPointer();
        asUint16.reserve(dimsData.getElementCount());
        asUint16.assign(ptrUint16, ptrUint16 + dimsData.getElementCount());
        fullySerialized = true;
    } break;
    case NLS_INT16: {
        int16* ptrInt16 = (int16*)data.getDataPointer();
        asInt16.reserve(dimsData.getElementCount());
        asInt16.assign(ptrInt16, ptrInt16 + dimsData.getElementCount());
        fullySerialized = true;
    } break;
    case NLS_UINT32: {
        uint32* ptrUint32 = (uint32*)data.getDataPointer();
        asUint32.reserve(dimsData.getElementCount());
        asUint32.assign(ptrUint32, ptrUint32 + dimsData.getElementCount());
        fullySerialized = true;
    } break;
    case NLS_INT32: {
        int32* ptrInt32 = (int32*)data.getDataPointer();
        asInt32.reserve(dimsData.getElementCount());
        asInt32.assign(ptrInt32, ptrInt32 + dimsData.getElementCount());
        fullySerialized = true;
    } break;
    case NLS_UINT64: {
        uint64* ptrUint64 = (uint64*)data.getDataPointer();
        asUint64.reserve(dimsData.getElementCount());
        asUint64.assign(ptrUint64, ptrUint64 + dimsData.getElementCount());
        fullySerialized = true;
    } break;
    case NLS_INT64: {
        int64* ptrInt64 = (int64*)data.getDataPointer();
        asInt64.reserve(dimsData.getElementCount());
        asInt64.assign(ptrInt64, ptrInt64 + dimsData.getElementCount());
        fullySerialized = true;
    } break;
    case NLS_SINGLE: {
        single* ptrSingle = (single*)data.getDataPointer();
        asSingle.reserve(dimsData.getElementCount());
        asSingle.assign(ptrSingle, ptrSingle + dimsData.getElementCount());
        fullySerialized = true;
    } break;
    case NLS_DOUBLE: {
        if (!isSparse) {
            double* ptrDouble = (double*)data.getDataPointer();
            asDouble.reserve(dimsData.getElementCount());
            asDouble.assign(ptrDouble, ptrDouble + dimsData.getElementCount());
        } else {
            const Eigen::SparseMatrix<double, 0, signedIndexType>* spMat
                = (Eigen::SparseMatrix<double, 0, signedIndexType>*)data.getSparseDataPointer();
            if (spMat) {
                eigenSparseToIJV<double>(*spMat, I, J, asDouble, nzmax);
            }
        }
        fullySerialized = true;
    } break;
    case NLS_SCOMPLEX: {
        single* ptrSingle = (single*)data.getDataPointer();
        asSingle.reserve(dimsData.getElementCount() * 2);
        asSingle.assign(ptrSingle, ptrSingle + (dimsData.getElementCount() * 2));
        fullySerialized = true;
    } break;
    case NLS_DCOMPLEX: {
        if (!isSparse) {
            double* ptrDouble = (double*)data.getDataPointer();
            asDouble.reserve(dimsData.getElementCount() * 2);
            asDouble.assign(ptrDouble, ptrDouble + (dimsData.getElementCount() * 2));
        } else {
            const Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>* spMat
                = (Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>*)
                      data.getSparseDataPointer();
            if (spMat) {
                std::vector<doublecomplex> V;
                eigenSparseToIJV<doublecomplex>(*spMat, I, J, V, nzmax);
                double* Vz = reinterpret_cast<double*>(V.data());
                asDouble.reserve(V.size() * 2);
                asDouble.assign(Vz, Vz + (V.size() * 2));
            }
        }
        fullySerialized = true;
    } break;
    case NLS_MISSING_ARRAY: {
        double* ptrDouble = (double*)data.getDataPointer();
        asDouble.reserve(dimsData.getElementCount());
        asDouble.assign(ptrDouble, ptrDouble + dimsData.getElementCount());
        fullySerialized = true;
    } break;

    case NLS_CHAR: {
        charType* ptrCharacter = (charType*)data.getDataPointer();
        asCharacter.reserve(dimsData.getElementCount());
        asCharacter.assign(ptrCharacter, ptrCharacter + dimsData.getElementCount());
        fullySerialized = true;
    } break;
    case NLS_GO_HANDLE:
    case NLS_HANDLE:
    default: {
        fullySerialized = false;
    } break;
    }
    return fullySerialized;
}
//=============================================================================
ArrayOf
ArrayOfSerialization::get(bool& success)
{
    ArrayOf res;
    NelsonType destinationClass = (NelsonType)nelsonObjectClass;
    Dimensions destinationDims(dims);
    switch (destinationClass) {
    case NLS_GO_HANDLE: {
        success = false;
    } break;
    case NLS_HANDLE: {
        success = false;
    } break;
    case NLS_CELL_ARRAY:
    case NLS_STRING_ARRAY:
    case NLS_CLASS_ARRAY:
    case NLS_FUNCTION_HANDLE:
    case NLS_STRUCT_ARRAY: {
        indexType nbElements = destinationDims.getElementCount();
        ArrayOf* ptrArrayOf
            = (ArrayOf*)ArrayOf::allocateArrayOf(destinationClass, nbElements, fieldnames);
        res = ArrayOf(destinationClass, destinationDims, ptrArrayOf, isSparse, fieldnames);
        for (indexType k = 0; k < nbElements; ++k) {
            ptrArrayOf[k] = subArrayOf[k].get(success);
        }
    } break;
    case NLS_LOGICAL: {
        if (!isSparse) {
            uint8* ptrUInt8
                = (uint8*)ArrayOf::allocateArrayOf(NLS_LOGICAL, destinationDims.getElementCount());
            res = ArrayOf(NLS_LOGICAL, destinationDims, ptrUInt8, isSparse);
            memcpy(ptrUInt8, asUint8.data(), sizeof(uint8) * asUint8.size());
        } else {
            res = ArrayOf(NLS_LOGICAL, destinationDims,
                IJVToAllocatedEigenSparse<logical>(
                    I, J, asUint8, destinationDims.getRows(), destinationDims.getColumns(), nzmax),
                isSparse);
        }
        success = true;
    } break;
    case NLS_UINT8: {
        uint8* ptrUInt8
            = (uint8*)ArrayOf::allocateArrayOf(NLS_UINT8, destinationDims.getElementCount());
        res = ArrayOf(NLS_UINT8, destinationDims, ptrUInt8, isSparse);
        memcpy(ptrUInt8, asUint8.data(), sizeof(uint8) * asUint8.size());
        success = true;
    } break;
    case NLS_INT8: {
        int8* ptrInt8
            = (int8*)ArrayOf::allocateArrayOf(NLS_INT8, destinationDims.getElementCount());
        res = ArrayOf(NLS_INT8, destinationDims, ptrInt8, isSparse);
        memcpy(ptrInt8, asInt8.data(), sizeof(int8) * asInt8.size());
        success = true;
    } break;
    case NLS_UINT16: {
        uint16* ptrUInt16
            = (uint16*)ArrayOf::allocateArrayOf(NLS_UINT16, destinationDims.getElementCount());
        res = ArrayOf(NLS_UINT16, destinationDims, ptrUInt16, isSparse);
        memcpy(ptrUInt16, asUint16.data(), sizeof(uint16) * asUint16.size());
        success = true;
    } break;
    case NLS_INT16: {
        int16* ptrInt16
            = (int16*)ArrayOf::allocateArrayOf(NLS_INT16, destinationDims.getElementCount());
        res = ArrayOf(NLS_INT16, destinationDims, ptrInt16, isSparse);
        memcpy(ptrInt16, asInt16.data(), sizeof(int16) * asInt16.size());
        success = true;
    } break;
    case NLS_UINT32: {
        uint32* ptrUInt32
            = (uint32*)ArrayOf::allocateArrayOf(NLS_UINT32, destinationDims.getElementCount());
        res = ArrayOf(NLS_UINT32, destinationDims, ptrUInt32, isSparse);
        memcpy(ptrUInt32, asUint32.data(), sizeof(uint32) * asUint32.size());
        success = true;
    } break;
    case NLS_INT32: {
        int32* ptrInt32
            = (int32*)ArrayOf::allocateArrayOf(NLS_INT32, destinationDims.getElementCount());
        res = ArrayOf(NLS_INT32, destinationDims, ptrInt32, isSparse);
        memcpy(ptrInt32, asInt32.data(), sizeof(int32) * asInt32.size());
        success = true;
    } break;
    case NLS_UINT64: {
        uint64* ptrUInt64
            = (uint64*)ArrayOf::allocateArrayOf(NLS_UINT64, destinationDims.getElementCount());
        res = ArrayOf(NLS_UINT64, destinationDims, ptrUInt64, isSparse);
        memcpy(ptrUInt64, asUint64.data(), sizeof(uint64) * asUint64.size());
        success = true;
    } break;
    case NLS_INT64: {
        int64* ptrInt64
            = (int64*)ArrayOf::allocateArrayOf(NLS_INT64, destinationDims.getElementCount());
        res = ArrayOf(NLS_INT64, destinationDims, ptrInt64, isSparse);
        memcpy(ptrInt64, asInt64.data(), sizeof(int64) * asInt64.size());
        success = true;
    } break;
    case NLS_SINGLE: {
        single* ptrSingle
            = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, destinationDims.getElementCount());
        res = ArrayOf(NLS_SINGLE, destinationDims, ptrSingle, isSparse);
        memcpy(ptrSingle, asSingle.data(), sizeof(single) * asSingle.size());
        success = true;
    } break;
    case NLS_DOUBLE: {
        if (!isSparse) {
            double* ptrDouble
                = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, destinationDims.getElementCount());
            res = ArrayOf(NLS_DOUBLE, destinationDims, ptrDouble, isSparse);
            memcpy(ptrDouble, asDouble.data(), sizeof(double) * asDouble.size());
        } else {
            res = ArrayOf(NLS_DOUBLE, destinationDims,
                IJVToAllocatedEigenSparse<double>(
                    I, J, asDouble, destinationDims.getRows(), destinationDims.getColumns(), nzmax),
                isSparse);
        }
        success = true;
    } break;
    case NLS_SCOMPLEX: {
        single* ptrSingle
            = (single*)ArrayOf::allocateArrayOf(NLS_SCOMPLEX, destinationDims.getElementCount());
        res = ArrayOf(NLS_SCOMPLEX, destinationDims, ptrSingle, isSparse);
        memcpy(ptrSingle, asSingle.data(), sizeof(single) * asSingle.size());
        success = true;
    } break;
    case NLS_DCOMPLEX: {
        if (!isSparse) {
            double* ptrDouble = (double*)ArrayOf::allocateArrayOf(
                NLS_DCOMPLEX, destinationDims.getElementCount());
            res = ArrayOf(NLS_DCOMPLEX, destinationDims, ptrDouble, isSparse);
            memcpy(ptrDouble, asDouble.data(), sizeof(double) * asDouble.size());
        } else {
            auto* Vz = reinterpret_cast<doublecomplex*>(asDouble.data());
            std::vector<doublecomplex> V;
            V.reserve(asDouble.size() / 2);
            V.assign(Vz, Vz + (asDouble.size() / 2));
            res = ArrayOf(NLS_DCOMPLEX, destinationDims,
                IJVToAllocatedEigenSparse<doublecomplex>(
                    I, J, V, destinationDims.getRows(), destinationDims.getColumns(), nzmax),
                isSparse);
        }
        success = true;
    } break;
    case NLS_MISSING_ARRAY: {
        double* ptrDouble = (double*)ArrayOf::allocateArrayOf(
            NLS_MISSING_ARRAY, destinationDims.getElementCount());
        res = ArrayOf(NLS_MISSING_ARRAY, destinationDims, ptrDouble);
        memcpy(ptrDouble, asDouble.data(), sizeof(double) * asDouble.size());
        success = true;
    } break;
    case NLS_CHAR: {
        charType* ptrCharacter = (charType*)ArrayOf::allocateArrayOf(
            NLS_CHAR, destinationDims.getElementCount(), stringVector(), true);
        res = ArrayOf(NLS_CHAR, destinationDims, ptrCharacter, isSparse);
        memcpy(ptrCharacter, asCharacter.data(), sizeof(charType) * asCharacter.size());
        success = true;
    } break;
    default: {
        success = false;
    } break;
    }
    return res;
}
//=============================================================================
}
//=============================================================================
