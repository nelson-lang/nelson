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
#include "SparseType.hpp"
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static FunctionHandleSerializationEncoder functionHandleEncoder = nullptr;
static FunctionHandleSerializationDecoder functionHandleDecoder = nullptr;
//=============================================================================
void
registerFunctionHandleSerializers(
    FunctionHandleSerializationEncoder encoder, FunctionHandleSerializationDecoder decoder)
{
    functionHandleEncoder = encoder;
    functionHandleDecoder = decoder;
}
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
std::vector<indexType>
toSparseConstructorIndices(const std::vector<uint64>& values)
{
    std::vector<indexType> indices;
    indices.reserve(values.size());
    for (uint64 value : values) {
        indices.push_back(static_cast<indexType>(value + 1));
    }
    return indices;
}
//=============================================================================
void*
IJVToSparseMatrix(NelsonType dclass, const std::vector<uint64>& I, const std::vector<uint64>& J,
    const void* V, indexType rows, indexType cols)
{
    std::vector<indexType> rowsIndex = toSparseConstructorIndices(I);
    std::vector<indexType> colsIndex = toSparseConstructorIndices(J);
    // I, J and V are parallel per-nonzero arrays, so each uses stride 1. Stride 0
    // would reindex every triplet to (I[0], J[0]) and corrupt the matrix.
    return Eigen_makeSparseFromIJV(dclass, rows, cols, static_cast<indexType>(rowsIndex.size()),
        rowsIndex.data(), 1, colsIndex.data(), 1, V, 1, false);
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
    classTypeName.clear();
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
    if (data.dataClass() == NLS_FUNCTION_HANDLE && functionHandleEncoder != nullptr) {
        return set(functionHandleEncoder(data));
    }
    nelsonObjectClass = (int)data.dataClass();
    isSparse = data.isSparse();
    if (data.isClassType()) {
        classTypeName = data.classType();
    }
    Dimensions dimsData = data.dimensions();
    dims = dimsData.asVector();
    switch (data.dataClass()) {
    case NLS_CELL_ARRAY:
    case NLS_STRING_ARRAY:
    case NLS_CLASS_ARRAY:
    case NLS_CLASSDEF_ARRAY:
    case NLS_FUNCTION_HANDLE:
    case NLS_STRUCT_ARRAY: {
        ArrayOf* elements = (ArrayOf*)data.dataPointer();
        fieldnames = data.fieldNames();
        indexType nbElements = dimsData.elementCount();
        if (data.dataClass() == NLS_STRUCT_ARRAY || data.dataClass() == NLS_CLASS_ARRAY
            || data.dataClass() == NLS_CLASSDEF_ARRAY || data.dataClass() == NLS_FUNCTION_HANDLE) {
            nbElements *= static_cast<indexType>(fieldnames.size());
        }
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
            logical* ptrLogical = (logical*)data.dataPointer();
            asUint8.reserve(dimsData.elementCount());
            asUint8.assign(ptrLogical, ptrLogical + dimsData.elementCount());
        } else {
            const Eigen::SparseMatrix<uint8, 0, signedIndexType>* spMat
                = (Eigen::SparseMatrix<uint8, 0, signedIndexType>*)data.sparseDataPointer();
            if (spMat) {
                eigenSparseToIJV<uint8>(*spMat, I, J, asUint8, nzmax);
            }
        }
        fullySerialized = true;
    } break;
    case NLS_UINT8: {
        uint8* ptrUint8 = (uint8*)data.dataPointer();
        asUint8.reserve(dimsData.elementCount());
        asUint8.assign(ptrUint8, ptrUint8 + dimsData.elementCount());
        fullySerialized = true;
    } break;
    case NLS_INT8: {
        int8* ptrInt8 = (int8*)data.dataPointer();
        asInt8.reserve(dimsData.elementCount());
        asInt8.assign(ptrInt8, ptrInt8 + dimsData.elementCount());
        fullySerialized = true;
    } break;
    case NLS_UINT16: {
        uint16* ptrUint16 = (uint16*)data.dataPointer();
        asUint16.reserve(dimsData.elementCount());
        asUint16.assign(ptrUint16, ptrUint16 + dimsData.elementCount());
        fullySerialized = true;
    } break;
    case NLS_INT16: {
        int16* ptrInt16 = (int16*)data.dataPointer();
        asInt16.reserve(dimsData.elementCount());
        asInt16.assign(ptrInt16, ptrInt16 + dimsData.elementCount());
        fullySerialized = true;
    } break;
    case NLS_UINT32: {
        uint32* ptrUint32 = (uint32*)data.dataPointer();
        asUint32.reserve(dimsData.elementCount());
        asUint32.assign(ptrUint32, ptrUint32 + dimsData.elementCount());
        fullySerialized = true;
    } break;
    case NLS_INT32: {
        int32* ptrInt32 = (int32*)data.dataPointer();
        asInt32.reserve(dimsData.elementCount());
        asInt32.assign(ptrInt32, ptrInt32 + dimsData.elementCount());
        fullySerialized = true;
    } break;
    case NLS_UINT64: {
        uint64* ptrUint64 = (uint64*)data.dataPointer();
        asUint64.reserve(dimsData.elementCount());
        asUint64.assign(ptrUint64, ptrUint64 + dimsData.elementCount());
        fullySerialized = true;
    } break;
    case NLS_INT64: {
        int64* ptrInt64 = (int64*)data.dataPointer();
        asInt64.reserve(dimsData.elementCount());
        asInt64.assign(ptrInt64, ptrInt64 + dimsData.elementCount());
        fullySerialized = true;
    } break;
    case NLS_SINGLE: {
        single* ptrSingle = (single*)data.dataPointer();
        asSingle.reserve(dimsData.elementCount());
        asSingle.assign(ptrSingle, ptrSingle + dimsData.elementCount());
        fullySerialized = true;
    } break;
    case NLS_DOUBLE: {
        if (!isSparse) {
            double* ptrDouble = (double*)data.dataPointer();
            asDouble.reserve(dimsData.elementCount());
            asDouble.assign(ptrDouble, ptrDouble + dimsData.elementCount());
        } else {
            const Eigen::SparseMatrix<double, 0, signedIndexType>* spMat
                = (Eigen::SparseMatrix<double, 0, signedIndexType>*)data.sparseDataPointer();
            if (spMat) {
                eigenSparseToIJV<double>(*spMat, I, J, asDouble, nzmax);
            }
        }
        fullySerialized = true;
    } break;
    case NLS_SCOMPLEX: {
        single* ptrSingle = (single*)data.dataPointer();
        asSingle.reserve(dimsData.elementCount() * 2);
        asSingle.assign(ptrSingle, ptrSingle + (dimsData.elementCount() * 2));
        fullySerialized = true;
    } break;
    case NLS_DCOMPLEX: {
        if (!isSparse) {
            double* ptrDouble = (double*)data.dataPointer();
            asDouble.reserve(dimsData.elementCount() * 2);
            asDouble.assign(ptrDouble, ptrDouble + (dimsData.elementCount() * 2));
        } else {
            const Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>* spMat
                = (Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>*)data.sparseDataPointer();
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
        double* ptrDouble = (double*)data.dataPointer();
        asDouble.reserve(dimsData.elementCount());
        asDouble.assign(ptrDouble, ptrDouble + dimsData.elementCount());
        fullySerialized = true;
    } break;

    case NLS_CHAR: {
        charType* ptrCharacter = (charType*)data.dataPointer();
        asCharacter.reserve(dimsData.elementCount());
        asCharacter.assign(ptrCharacter, ptrCharacter + dimsData.elementCount());
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
    case NLS_CLASSDEF_ARRAY:
    case NLS_FUNCTION_HANDLE:
    case NLS_STRUCT_ARRAY: {
        indexType nbElements = destinationDims.elementCount();
        ArrayOf* ptrArrayOf
            = (ArrayOf*)ArrayOf::allocateArrayOf(destinationClass, nbElements, fieldnames);
        res = ArrayOf(destinationClass, destinationDims, ptrArrayOf, isSparse, fieldnames);
        if (res.isClassType()) {
            res.setClassType(classTypeName);
        }
        success = true;
        if (destinationClass == NLS_STRUCT_ARRAY || destinationClass == NLS_CLASS_ARRAY
            || destinationClass == NLS_CLASSDEF_ARRAY || destinationClass == NLS_FUNCTION_HANDLE) {
            nbElements *= static_cast<indexType>(fieldnames.size());
        }
        for (indexType k = 0; k < nbElements; ++k) {
            bool itemSuccess = false;
            ptrArrayOf[k] = subArrayOf[k].get(itemSuccess);
            if (!itemSuccess) {
                success = false;
            }
        }
        if (success && destinationClass == NLS_CLASS_ARRAY
            && classTypeName == NLS_FUNCTION_HANDLE_SERIALIZATION_MARKER
            && functionHandleDecoder != nullptr) {
            bool decoded = false;
            ArrayOf rebuilt = functionHandleDecoder(res, decoded);
            if (decoded) {
                res = rebuilt;
            }
        }
    } break;
    case NLS_LOGICAL: {
        if (!isSparse) {
            uint8* ptrUInt8
                = (uint8*)ArrayOf::allocateArrayOf(NLS_LOGICAL, destinationDims.elementCount());
            res = ArrayOf(NLS_LOGICAL, destinationDims, ptrUInt8, isSparse);
            memcpy(ptrUInt8, asUint8.data(), sizeof(uint8) * asUint8.size());
        } else {
            res = ArrayOf(NLS_LOGICAL, destinationDims,
                IJVToSparseMatrix(NLS_LOGICAL, I, J, asUint8.data(), destinationDims.rows(),
                    destinationDims.columns()),
                isSparse);
            res.setSparseNzmax(static_cast<indexType>(nzmax));
        }
        success = true;
    } break;
    case NLS_UINT8: {
        uint8* ptrUInt8
            = (uint8*)ArrayOf::allocateArrayOf(NLS_UINT8, destinationDims.elementCount());
        res = ArrayOf(NLS_UINT8, destinationDims, ptrUInt8, isSparse);
        memcpy(ptrUInt8, asUint8.data(), sizeof(uint8) * asUint8.size());
        success = true;
    } break;
    case NLS_INT8: {
        int8* ptrInt8 = (int8*)ArrayOf::allocateArrayOf(NLS_INT8, destinationDims.elementCount());
        res = ArrayOf(NLS_INT8, destinationDims, ptrInt8, isSparse);
        memcpy(ptrInt8, asInt8.data(), sizeof(int8) * asInt8.size());
        success = true;
    } break;
    case NLS_UINT16: {
        uint16* ptrUInt16
            = (uint16*)ArrayOf::allocateArrayOf(NLS_UINT16, destinationDims.elementCount());
        res = ArrayOf(NLS_UINT16, destinationDims, ptrUInt16, isSparse);
        memcpy(ptrUInt16, asUint16.data(), sizeof(uint16) * asUint16.size());
        success = true;
    } break;
    case NLS_INT16: {
        int16* ptrInt16
            = (int16*)ArrayOf::allocateArrayOf(NLS_INT16, destinationDims.elementCount());
        res = ArrayOf(NLS_INT16, destinationDims, ptrInt16, isSparse);
        memcpy(ptrInt16, asInt16.data(), sizeof(int16) * asInt16.size());
        success = true;
    } break;
    case NLS_UINT32: {
        uint32* ptrUInt32
            = (uint32*)ArrayOf::allocateArrayOf(NLS_UINT32, destinationDims.elementCount());
        res = ArrayOf(NLS_UINT32, destinationDims, ptrUInt32, isSparse);
        memcpy(ptrUInt32, asUint32.data(), sizeof(uint32) * asUint32.size());
        success = true;
    } break;
    case NLS_INT32: {
        int32* ptrInt32
            = (int32*)ArrayOf::allocateArrayOf(NLS_INT32, destinationDims.elementCount());
        res = ArrayOf(NLS_INT32, destinationDims, ptrInt32, isSparse);
        memcpy(ptrInt32, asInt32.data(), sizeof(int32) * asInt32.size());
        success = true;
    } break;
    case NLS_UINT64: {
        uint64* ptrUInt64
            = (uint64*)ArrayOf::allocateArrayOf(NLS_UINT64, destinationDims.elementCount());
        res = ArrayOf(NLS_UINT64, destinationDims, ptrUInt64, isSparse);
        memcpy(ptrUInt64, asUint64.data(), sizeof(uint64) * asUint64.size());
        success = true;
    } break;
    case NLS_INT64: {
        int64* ptrInt64
            = (int64*)ArrayOf::allocateArrayOf(NLS_INT64, destinationDims.elementCount());
        res = ArrayOf(NLS_INT64, destinationDims, ptrInt64, isSparse);
        memcpy(ptrInt64, asInt64.data(), sizeof(int64) * asInt64.size());
        success = true;
    } break;
    case NLS_SINGLE: {
        single* ptrSingle
            = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, destinationDims.elementCount());
        res = ArrayOf(NLS_SINGLE, destinationDims, ptrSingle, isSparse);
        memcpy(ptrSingle, asSingle.data(), sizeof(single) * asSingle.size());
        success = true;
    } break;
    case NLS_DOUBLE: {
        if (!isSparse) {
            double* ptrDouble
                = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, destinationDims.elementCount());
            res = ArrayOf(NLS_DOUBLE, destinationDims, ptrDouble, isSparse);
            memcpy(ptrDouble, asDouble.data(), sizeof(double) * asDouble.size());
        } else {
            res = ArrayOf(NLS_DOUBLE, destinationDims,
                IJVToSparseMatrix(NLS_DOUBLE, I, J, asDouble.data(), destinationDims.rows(),
                    destinationDims.columns()),
                isSparse);
            res.setSparseNzmax(static_cast<indexType>(nzmax));
        }
        success = true;
    } break;
    case NLS_SCOMPLEX: {
        single* ptrSingle
            = (single*)ArrayOf::allocateArrayOf(NLS_SCOMPLEX, destinationDims.elementCount());
        res = ArrayOf(NLS_SCOMPLEX, destinationDims, ptrSingle, isSparse);
        memcpy(ptrSingle, asSingle.data(), sizeof(single) * asSingle.size());
        success = true;
    } break;
    case NLS_DCOMPLEX: {
        if (!isSparse) {
            double* ptrDouble
                = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, destinationDims.elementCount());
            res = ArrayOf(NLS_DCOMPLEX, destinationDims, ptrDouble, isSparse);
            memcpy(ptrDouble, asDouble.data(), sizeof(double) * asDouble.size());
        } else {
            res = ArrayOf(NLS_DCOMPLEX, destinationDims,
                IJVToSparseMatrix(NLS_DCOMPLEX, I, J, asDouble.data(), destinationDims.rows(),
                    destinationDims.columns()),
                isSparse);
            res.setSparseNzmax(static_cast<indexType>(nzmax));
        }
        success = true;
    } break;
    case NLS_MISSING_ARRAY: {
        double* ptrDouble
            = (double*)ArrayOf::allocateArrayOf(NLS_MISSING_ARRAY, destinationDims.elementCount());
        res = ArrayOf(NLS_MISSING_ARRAY, destinationDims, ptrDouble);
        memcpy(ptrDouble, asDouble.data(), sizeof(double) * asDouble.size());
        success = true;
    } break;
    case NLS_CHAR: {
        charType* ptrCharacter = (charType*)ArrayOf::allocateArrayOf(
            NLS_CHAR, destinationDims.elementCount(), stringVector(), true);
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
void
ArrayOfSerialization::write(BinaryWriter& writer) const
{
    writer.writeU8(fullySerialized ? 1 : 0);
    writer.writePod<int32_t>(static_cast<int32_t>(nelsonObjectClass));
    writer.writeU8(isSparse ? 1 : 0);
    writer.writeString(classTypeName);
    writer.writeVector(dims);
    writer.writeStringVector(fieldnames);
    if (isSparse) {
        writer.writeVector(I);
        writer.writeVector(J);
        writer.writeU64(nzmax);
    }
    switch ((NelsonType)nelsonObjectClass) {
    case NLS_CELL_ARRAY:
    case NLS_CLASS_ARRAY:
    case NLS_CLASSDEF_ARRAY:
    case NLS_FUNCTION_HANDLE:
    case NLS_STRUCT_ARRAY:
    case NLS_STRING_ARRAY: {
        writer.writeU64(static_cast<uint64_t>(subArrayOf.size()));
        for (const ArrayOfSerialization& child : subArrayOf) {
            child.write(writer);
        }
    } break;
    case NLS_LOGICAL:
    case NLS_UINT8: {
        writer.writeVector(asUint8);
    } break;
    case NLS_INT8: {
        writer.writeVector(asInt8);
    } break;
    case NLS_UINT16: {
        writer.writeVector(asUint16);
    } break;
    case NLS_INT16: {
        writer.writeVector(asInt16);
    } break;
    case NLS_UINT32: {
        writer.writeVector(asUint32);
    } break;
    case NLS_INT32: {
        writer.writeVector(asInt32);
    } break;
    case NLS_UINT64: {
        writer.writeVector(asUint64);
    } break;
    case NLS_INT64: {
        writer.writeVector(asInt64);
    } break;
    case NLS_SCOMPLEX:
    case NLS_SINGLE: {
        writer.writeVector(asSingle);
    } break;
    case NLS_DCOMPLEX:
    case NLS_DOUBLE:
    case NLS_MISSING_ARRAY: {
        writer.writeVector(asDouble);
    } break;
    case NLS_CHAR: {
        writer.writeVector(asCharacter);
    } break;
    case NLS_UNKNOWN:
    case NLS_GO_HANDLE:
    case NLS_HANDLE:
    default: {
    } break;
    }
}
//=============================================================================
bool
ArrayOfSerialization::read(BinaryReader& reader)
{
    clear();
    uint8_t flag = 0;
    if (!reader.readU8(flag)) {
        return false;
    }
    fullySerialized = (flag != 0);
    int32_t objectClass = 0;
    if (!reader.readPod(objectClass)) {
        return false;
    }
    nelsonObjectClass = objectClass;
    if (!reader.readU8(flag)) {
        return false;
    }
    isSparse = (flag != 0);
    if (!reader.readString(classTypeName)) {
        return false;
    }
    if (!reader.readVector(dims)) {
        return false;
    }
    if (!reader.readStringVector(fieldnames)) {
        return false;
    }
    if (isSparse) {
        if (!reader.readVector(I) || !reader.readVector(J) || !reader.readU64(nzmax)) {
            return false;
        }
    }
    switch ((NelsonType)nelsonObjectClass) {
    case NLS_CELL_ARRAY:
    case NLS_CLASS_ARRAY:
    case NLS_CLASSDEF_ARRAY:
    case NLS_FUNCTION_HANDLE:
    case NLS_STRUCT_ARRAY:
    case NLS_STRING_ARRAY: {
        uint64_t count = 0;
        if (!reader.readU64(count)) {
            return false;
        }
        subArrayOf.resize(static_cast<size_t>(count));
        for (uint64_t k = 0; k < count; ++k) {
            if (!subArrayOf[static_cast<size_t>(k)].read(reader)) {
                return false;
            }
        }
    } break;
    case NLS_LOGICAL:
    case NLS_UINT8: {
        return reader.readVector(asUint8);
    }
    case NLS_INT8: {
        return reader.readVector(asInt8);
    }
    case NLS_UINT16: {
        return reader.readVector(asUint16);
    }
    case NLS_INT16: {
        return reader.readVector(asInt16);
    }
    case NLS_UINT32: {
        return reader.readVector(asUint32);
    }
    case NLS_INT32: {
        return reader.readVector(asInt32);
    }
    case NLS_UINT64: {
        return reader.readVector(asUint64);
    }
    case NLS_INT64: {
        return reader.readVector(asInt64);
    }
    case NLS_SCOMPLEX:
    case NLS_SINGLE: {
        return reader.readVector(asSingle);
    }
    case NLS_DCOMPLEX:
    case NLS_DOUBLE:
    case NLS_MISSING_ARRAY: {
        return reader.readVector(asDouble);
    }
    case NLS_CHAR: {
        return reader.readVector(asCharacter);
    }
    case NLS_UNKNOWN:
    case NLS_GO_HANDLE:
    case NLS_HANDLE:
    default: {
    } break;
    }
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
