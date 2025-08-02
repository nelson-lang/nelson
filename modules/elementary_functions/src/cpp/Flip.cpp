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
#include <Eigen/Dense>
#include "Flip.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
void
TFlipLR2dReal(T* ptrIn, T* ptrOut, indexType m, indexType n)
{
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matIn(ptrIn, m, n);
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matOut(ptrOut, m, n);
    OMP_PARALLEL_FOR_LOOP(m * n)
    for (ompIndexType j = 0; j < (ompIndexType)(m * n); ++j) {
        ompIndexType row = j % m;
        ompIndexType col = j / m;
        ompIndexType flippedCol = n - 1 - col;
        matOut(row, col) = matIn(row, flippedCol);
    }
}
//=============================================================================
template <class T>
void
TFlipLR2dComplex(T* ptrIn, T* ptrOut, indexType m, indexType n)
{
    auto* Az = reinterpret_cast<std::complex<T>*>(ptrIn);
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matIn(Az, m, n);
    auto* Cz = reinterpret_cast<std::complex<T>*>(ptrOut);
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matOut(Cz, m, n);
    OMP_PARALLEL_FOR_LOOP(m * n)
    for (ompIndexType j = 0; j < (ompIndexType)(m * n); ++j) {
        ompIndexType row = j % m;
        ompIndexType col = j / m;
        ompIndexType flippedCol = n - 1 - col;
        matOut(row, col) = matIn(row, flippedCol);
    }
}
//=============================================================================
template <class T>
void
TFlipUD2dReal(T* ptrIn, T* ptrOut, indexType m, indexType n)
{
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matIn(ptrIn, m, n);
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matOut(ptrOut, m, n);
    OMP_PARALLEL_FOR_LOOP(m * n)
    for (ompIndexType idx = 0; idx < (ompIndexType)(m * n); ++idx) {
        ompIndexType j = idx / m;
        ompIndexType i = idx % m;
        matOut(i, j) = matIn(m - 1 - i, j);
    }
}
//=============================================================================
template <class T>
void
TFlipUD2dComplex(T* ptrIn, T* ptrOut, indexType m, indexType n)
{
    auto* Az = reinterpret_cast<std::complex<T>*>(ptrIn);
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matIn(Az, m, n);
    auto* Cz = reinterpret_cast<std::complex<T>*>(ptrOut);
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matOut(Cz, m, n);
    OMP_PARALLEL_FOR_LOOP(m * n)
    for (ompIndexType idx = 0; idx < (ompIndexType)(m * n); ++idx) {
        ompIndexType j = idx / m;
        ompIndexType i = idx % m;
        matOut(i, j) = matIn(m - 1 - i, j);
    }
}
//=============================================================================
ArrayOf
Fliplr(const ArrayOf& arrayIn, bool& needToOverload)
{
    ArrayOf res;
    if (arrayIn.isEmpty() || arrayIn.isScalar()) {
        needToOverload = false;
        return arrayIn;
    }
    if (arrayIn.isSparse() || !arrayIn.is2D()) {
        needToOverload = true;
        return res;
    }
    Dimensions dims = arrayIn.getDimensions();
    indexType elementCount = dims.getElementCount();
    NelsonType outType = arrayIn.getDataClass();
    switch (outType) {
    case NLS_HANDLE:
    case NLS_CELL_ARRAY:
    case NLS_STRUCT_ARRAY:
    case NLS_CLASS_ARRAY:
    case NLS_FUNCTION_HANDLE:
    case NLS_STRING_ARRAY:
    default: {
        needToOverload = true;
    } break;
    case NLS_LOGICAL: {
        needToOverload = false;
        logical* ptr = static_cast<logical*>(ArrayOf::allocateArrayOf(outType, elementCount));
        res = ArrayOf(outType, dims, ptr);
        TFlipLR2dReal<logical>(
            (logical*)arrayIn.getDataPointer(), ptr, dims.getRows(), dims.getColumns());
    } break;
    case NLS_UINT8: {
        needToOverload = false;
        uint8* ptr = static_cast<uint8*>(ArrayOf::allocateArrayOf(outType, elementCount));
        res = ArrayOf(outType, dims, ptr);
        TFlipLR2dReal<uint8>(
            (uint8*)arrayIn.getDataPointer(), ptr, dims.getRows(), dims.getColumns());
    } break;
    case NLS_INT8: {
        needToOverload = false;
        int8* ptr = static_cast<int8*>(ArrayOf::allocateArrayOf(outType, elementCount));
        res = ArrayOf(outType, dims, ptr);
        TFlipLR2dReal<int8>(
            (int8*)arrayIn.getDataPointer(), ptr, dims.getRows(), dims.getColumns());
    } break;
    case NLS_UINT16: {
        needToOverload = false;
        uint16* ptr = static_cast<uint16*>(ArrayOf::allocateArrayOf(outType, elementCount));
        res = ArrayOf(outType, dims, ptr);
        TFlipLR2dReal<uint16>(
            (uint16*)arrayIn.getDataPointer(), ptr, dims.getRows(), dims.getColumns());
    } break;
    case NLS_INT16: {
        needToOverload = false;
        int16* ptr = static_cast<int16*>(ArrayOf::allocateArrayOf(outType, elementCount));
        res = ArrayOf(outType, dims, ptr);
        TFlipLR2dReal<int16>(
            (int16*)arrayIn.getDataPointer(), ptr, dims.getRows(), dims.getColumns());
    } break;
    case NLS_UINT32: {
        needToOverload = false;
        uint32* ptr = static_cast<uint32*>(ArrayOf::allocateArrayOf(outType, elementCount));
        res = ArrayOf(outType, dims, ptr);
        TFlipLR2dReal<uint32>(
            (uint32*)arrayIn.getDataPointer(), ptr, dims.getRows(), dims.getColumns());
    } break;
    case NLS_INT32: {
        needToOverload = false;
        int32* ptr = static_cast<int32*>(ArrayOf::allocateArrayOf(outType, elementCount));
        res = ArrayOf(outType, dims, ptr);
        TFlipLR2dReal<int32>(
            (int32*)arrayIn.getDataPointer(), ptr, dims.getRows(), dims.getColumns());
    } break;
    case NLS_UINT64: {
        needToOverload = false;
        uint64* ptr = static_cast<uint64*>(ArrayOf::allocateArrayOf(outType, elementCount));
        res = ArrayOf(outType, dims, ptr);
        TFlipLR2dReal<uint64>(
            (uint64*)arrayIn.getDataPointer(), ptr, dims.getRows(), dims.getColumns());
    } break;
    case NLS_INT64: {
        needToOverload = false;
        int64* ptr = static_cast<int64*>(ArrayOf::allocateArrayOf(outType, elementCount));
        res = ArrayOf(outType, dims, ptr);
        TFlipLR2dReal<int64>(
            (int64*)arrayIn.getDataPointer(), ptr, dims.getRows(), dims.getColumns());
    } break;
    case NLS_SINGLE: {
        needToOverload = false;
        single* ptr = static_cast<single*>(ArrayOf::allocateArrayOf(outType, elementCount));
        res = ArrayOf(outType, dims, ptr);
        TFlipLR2dReal<single>(
            (single*)arrayIn.getDataPointer(), ptr, dims.getRows(), dims.getColumns());
    } break;
    case NLS_DOUBLE: {
        needToOverload = false;
        double* ptr = static_cast<double*>(ArrayOf::allocateArrayOf(outType, elementCount));
        res = ArrayOf(outType, dims, ptr);
        TFlipLR2dReal<double>(
            (double*)arrayIn.getDataPointer(), ptr, dims.getRows(), dims.getColumns());
    } break;
    case NLS_SCOMPLEX: {
        needToOverload = false;
        single* ptr = static_cast<single*>(ArrayOf::allocateArrayOf(outType, elementCount));
        res = ArrayOf(outType, dims, ptr);
        TFlipLR2dComplex<single>(
            (single*)arrayIn.getDataPointer(), ptr, dims.getRows(), dims.getColumns());
    } break;
    case NLS_DCOMPLEX: {
        needToOverload = false;
        double* ptr = static_cast<double*>(ArrayOf::allocateArrayOf(outType, elementCount));
        res = ArrayOf(outType, dims, ptr);
        TFlipLR2dComplex<double>(
            (double*)arrayIn.getDataPointer(), ptr, dims.getRows(), dims.getColumns());
    } break;
    case NLS_CHAR: {
        needToOverload = false;
        charType* ptr = static_cast<charType*>(ArrayOf::allocateArrayOf(outType, elementCount));
        res = ArrayOf(outType, dims, ptr);
        TFlipLR2dReal<charType>(
            (charType*)arrayIn.getDataPointer(), ptr, dims.getRows(), dims.getColumns());
    } break;
    }
    return res;
}
//=============================================================================
ArrayOf
Flipud(const ArrayOf& arrayIn, bool& needToOverload)
{
    ArrayOf res;
    if (arrayIn.isEmpty()) {
        needToOverload = false;
        return arrayIn;
    }
    if (arrayIn.isSparse() || !arrayIn.is2D()) {
        needToOverload = true;
        return res;
    }
    Dimensions dims = arrayIn.getDimensions();
    indexType elementCount = dims.getElementCount();
    NelsonType outType = arrayIn.getDataClass();
    switch (outType) {
    case NLS_HANDLE:
    case NLS_CELL_ARRAY:
    case NLS_STRUCT_ARRAY:
    case NLS_CLASS_ARRAY:
    case NLS_FUNCTION_HANDLE:
    case NLS_STRING_ARRAY:
    default: {
        needToOverload = true;
    } break;
    case NLS_LOGICAL: {
        needToOverload = false;
        logical* ptr = static_cast<logical*>(const_cast<void*>(
            static_cast<const void*>(ArrayOf::allocateArrayOf(outType, elementCount))));
        res = ArrayOf(outType, dims, ptr);
        TFlipUD2dReal<logical>(
            (logical*)arrayIn.getDataPointer(), ptr, dims.getRows(), dims.getColumns());
    } break;
    case NLS_UINT8: {
        needToOverload = false;
        uint8* ptr = static_cast<uint8*>(const_cast<void*>(
            static_cast<const void*>(ArrayOf::allocateArrayOf(outType, elementCount))));
        res = ArrayOf(outType, dims, ptr);
        TFlipUD2dReal<uint8>(
            (uint8*)arrayIn.getDataPointer(), ptr, dims.getRows(), dims.getColumns());
    } break;
    case NLS_INT8: {
        needToOverload = false;
        int8* ptr = static_cast<int8*>(const_cast<void*>(
            static_cast<const void*>(ArrayOf::allocateArrayOf(outType, elementCount))));
        res = ArrayOf(outType, dims, ptr);
        TFlipUD2dReal<int8>(
            (int8*)arrayIn.getDataPointer(), ptr, dims.getRows(), dims.getColumns());
    } break;
    case NLS_UINT16: {
        needToOverload = false;
        uint16* ptr = static_cast<uint16*>(const_cast<void*>(
            static_cast<const void*>(ArrayOf::allocateArrayOf(outType, elementCount))));
        res = ArrayOf(outType, dims, ptr);
        TFlipUD2dReal<uint16>(
            (uint16*)arrayIn.getDataPointer(), ptr, dims.getRows(), dims.getColumns());
    } break;
    case NLS_INT16: {
        needToOverload = false;
        int16* ptr = static_cast<int16*>(const_cast<void*>(
            static_cast<const void*>(ArrayOf::allocateArrayOf(outType, elementCount))));
        res = ArrayOf(outType, dims, ptr);
        TFlipUD2dReal<int16>(
            (int16*)arrayIn.getDataPointer(), ptr, dims.getRows(), dims.getColumns());
    } break;
    case NLS_UINT32: {
        needToOverload = false;
        uint32* ptr = static_cast<uint32*>(const_cast<void*>(
            static_cast<const void*>(ArrayOf::allocateArrayOf(outType, elementCount))));
        res = ArrayOf(outType, dims, ptr);
        TFlipUD2dReal<uint32>(
            (uint32*)arrayIn.getDataPointer(), ptr, dims.getRows(), dims.getColumns());
    } break;
    case NLS_INT32: {
        needToOverload = false;
        int32* ptr = static_cast<int32*>(const_cast<void*>(
            static_cast<const void*>(ArrayOf::allocateArrayOf(outType, elementCount))));
        res = ArrayOf(outType, dims, ptr);
        TFlipUD2dReal<int32>(
            (int32*)arrayIn.getDataPointer(), ptr, dims.getRows(), dims.getColumns());
    } break;
    case NLS_UINT64: {
        needToOverload = false;
        uint64* ptr = static_cast<uint64*>(const_cast<void*>(
            static_cast<const void*>(ArrayOf::allocateArrayOf(outType, elementCount))));
        res = ArrayOf(outType, dims, ptr);
        TFlipUD2dReal<uint64>(
            (uint64*)arrayIn.getDataPointer(), ptr, dims.getRows(), dims.getColumns());
    } break;
    case NLS_INT64: {
        needToOverload = false;
        int64* ptr = static_cast<int64*>(const_cast<void*>(
            static_cast<const void*>(ArrayOf::allocateArrayOf(outType, elementCount))));
        res = ArrayOf(outType, dims, ptr);
        TFlipUD2dReal<int64>(
            (int64*)arrayIn.getDataPointer(), ptr, dims.getRows(), dims.getColumns());
    } break;
    case NLS_SINGLE: {
        needToOverload = false;
        single* ptr = static_cast<single*>(const_cast<void*>(
            static_cast<const void*>(ArrayOf::allocateArrayOf(outType, elementCount))));
        res = ArrayOf(outType, dims, ptr);
        TFlipUD2dReal<single>(
            (single*)arrayIn.getDataPointer(), ptr, dims.getRows(), dims.getColumns());
    } break;
    case NLS_DOUBLE: {
        needToOverload = false;
        double* ptr = static_cast<double*>(const_cast<void*>(
            static_cast<const void*>(ArrayOf::allocateArrayOf(outType, elementCount))));
        res = ArrayOf(outType, dims, ptr);
        TFlipUD2dReal<double>(
            (double*)arrayIn.getDataPointer(), ptr, dims.getRows(), dims.getColumns());
    } break;
    case NLS_SCOMPLEX: {
        needToOverload = false;
        single* ptr = static_cast<single*>(const_cast<void*>(
            static_cast<const void*>(ArrayOf::allocateArrayOf(outType, elementCount))));
        res = ArrayOf(outType, dims, ptr);
        TFlipUD2dComplex<single>(
            (single*)arrayIn.getDataPointer(), ptr, dims.getRows(), dims.getColumns());
    } break;
    case NLS_DCOMPLEX: {
        needToOverload = false;
        double* ptr = static_cast<double*>(const_cast<void*>(
            static_cast<const void*>(ArrayOf::allocateArrayOf(outType, elementCount))));
        res = ArrayOf(outType, dims, ptr);
        TFlipUD2dComplex<double>(
            (double*)arrayIn.getDataPointer(), ptr, dims.getRows(), dims.getColumns());
    } break;
    case NLS_CHAR: {
        needToOverload = false;
        charType* ptr = static_cast<charType*>(const_cast<void*>(
            static_cast<const void*>(ArrayOf::allocateArrayOf(outType, elementCount))));
        res = ArrayOf(outType, dims, ptr);
        TFlipUD2dReal<charType>(
            (charType*)arrayIn.getDataPointer(), ptr, dims.getRows(), dims.getColumns());
    } break;
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
