//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <Eigen/Dense>
#include "Flip.hpp"
#include "nlsConfig.h"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
void
TFlipLR2dReal(T* ptrIn, T* ptrOut, indexType m, indexType n)
{
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matIn(ptrIn, m, n);
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matOut(ptrOut, m, n);
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for 
#endif
    for (ompIndexType j = 0; j < (ompIndexType)n; ++j) {
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for 
#endif
        for (ompIndexType i = 0; i < (ompIndexType)m; ++i) {
            matOut(i, j) = matIn(i, n - 1 - j);
        }
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
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
    for (ompIndexType j = 0; j < (ompIndexType)n; ++j) {
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType i = 0; i < (ompIndexType)m; ++i) {
            matOut(i, j) = matIn(i, n - 1 - j);
        }
    }
}
//=============================================================================
template <class T>
void
TFlipUD2dReal(T* ptrIn, T* ptrOut, indexType m, indexType n)
{
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matIn(ptrIn, m, n);
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matOut(ptrOut, m, n);
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
    for (ompIndexType j = 0; j < (ompIndexType)n; ++j) {
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType i = 0; i < (ompIndexType)m; ++i) {
            matOut(i, j) = matIn(m - 1 - i, j);
        }
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
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
    for (ompIndexType j = 0; j < (ompIndexType)n; ++j) {
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType i = 0; i < (ompIndexType)m; ++i) {
            matOut(i, j) = matIn(m - 1 - i, j);
        }
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
    Class outType = arrayIn.getDataClass();
    switch (outType) {
    case NLS_HANDLE:
    case NLS_CELL_ARRAY:
    case NLS_STRUCT_ARRAY:
    case NLS_STRING_ARRAY:
    default: {
        needToOverload = true;
    } break;
    case NLS_LOGICAL: {
        needToOverload = false;
        logical* ptr = (logical*)ArrayOf::allocateArrayOf(outType, elementCount);
        res = ArrayOf(outType, dims, ptr);
        TFlipLR2dReal<logical>(
            (logical*)arrayIn.getDataPointer(), ptr, dims.getRows(), dims.getColumns());
    } break;
    case NLS_UINT8: {
        needToOverload = false;
        uint8* ptr = (uint8*)ArrayOf::allocateArrayOf(outType, elementCount);
        res = ArrayOf(outType, dims, ptr);
        TFlipLR2dReal<uint8>(
            (uint8*)arrayIn.getDataPointer(), ptr, dims.getRows(), dims.getColumns());
    } break;
    case NLS_INT8: {
        needToOverload = false;
        int8* ptr = (int8*)ArrayOf::allocateArrayOf(outType, elementCount);
        res = ArrayOf(outType, dims, ptr);
        TFlipLR2dReal<int8>(
            (int8*)arrayIn.getDataPointer(), ptr, dims.getRows(), dims.getColumns());
    } break;
    case NLS_UINT16: {
        needToOverload = false;
        uint16* ptr = (uint16*)ArrayOf::allocateArrayOf(outType, elementCount);
        res = ArrayOf(outType, dims, ptr);
        TFlipLR2dReal<uint16>(
            (uint16*)arrayIn.getDataPointer(), ptr, dims.getRows(), dims.getColumns());
    } break;
    case NLS_INT16: {
        needToOverload = false;
        int16* ptr = (int16*)ArrayOf::allocateArrayOf(outType, elementCount);
        res = ArrayOf(outType, dims, ptr);
        TFlipLR2dReal<int16>(
            (int16*)arrayIn.getDataPointer(), ptr, dims.getRows(), dims.getColumns());
    } break;
    case NLS_UINT32: {
        needToOverload = false;
        uint32* ptr = (uint32*)ArrayOf::allocateArrayOf(outType, elementCount);
        res = ArrayOf(outType, dims, ptr);
        TFlipLR2dReal<uint32>(
            (uint32*)arrayIn.getDataPointer(), ptr, dims.getRows(), dims.getColumns());
    } break;
    case NLS_INT32: {
        needToOverload = false;
        int32* ptr = (int32*)ArrayOf::allocateArrayOf(outType, elementCount);
        res = ArrayOf(outType, dims, ptr);
        TFlipLR2dReal<int32>(
            (int32*)arrayIn.getDataPointer(), ptr, dims.getRows(), dims.getColumns());
    } break;
    case NLS_UINT64: {
        needToOverload = false;
        uint64* ptr = (uint64*)ArrayOf::allocateArrayOf(outType, elementCount);
        res = ArrayOf(outType, dims, ptr);
        TFlipLR2dReal<uint64>(
            (uint64*)arrayIn.getDataPointer(), ptr, dims.getRows(), dims.getColumns());
    } break;
    case NLS_INT64: {
        needToOverload = false;
        int64* ptr = (int64*)ArrayOf::allocateArrayOf(outType, elementCount);
        res = ArrayOf(outType, dims, ptr);
        TFlipLR2dReal<int64>(
            (int64*)arrayIn.getDataPointer(), ptr, dims.getRows(), dims.getColumns());
    } break;
    case NLS_SINGLE: {
        needToOverload = false;
        single* ptr = (single*)ArrayOf::allocateArrayOf(outType, elementCount);
        res = ArrayOf(outType, dims, ptr);
        TFlipLR2dReal<single>(
            (single*)arrayIn.getDataPointer(), ptr, dims.getRows(), dims.getColumns());
    } break;
    case NLS_DOUBLE: {
        needToOverload = false;
        double* ptr = (double*)ArrayOf::allocateArrayOf(outType, elementCount);
        res = ArrayOf(outType, dims, ptr);
        TFlipLR2dReal<double>(
            (double*)arrayIn.getDataPointer(), ptr, dims.getRows(), dims.getColumns());
    } break;
    case NLS_SCOMPLEX: {
        needToOverload = false;
        single* ptr = (single*)ArrayOf::allocateArrayOf(outType, elementCount);
        res = ArrayOf(outType, dims, ptr);
        TFlipLR2dComplex<single>(
            (single*)arrayIn.getDataPointer(), ptr, dims.getRows(), dims.getColumns());
    } break;
    case NLS_DCOMPLEX: {
        needToOverload = false;
        double* ptr = (double*)ArrayOf::allocateArrayOf(outType, elementCount);
        res = ArrayOf(outType, dims, ptr);
        TFlipLR2dComplex<double>(
            (double*)arrayIn.getDataPointer(), ptr, dims.getRows(), dims.getColumns());
    } break;
    case NLS_CHAR: {
        needToOverload = false;
        charType* ptr = (charType*)ArrayOf::allocateArrayOf(outType, elementCount);
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
    Class outType = arrayIn.getDataClass();
    switch (outType) {
    case NLS_HANDLE:
    case NLS_CELL_ARRAY:
    case NLS_STRUCT_ARRAY:
    case NLS_STRING_ARRAY:
    default: {
        needToOverload = true;
    } break;
    case NLS_LOGICAL: {
        needToOverload = false;
        logical* ptr = (logical*)ArrayOf::allocateArrayOf(outType, elementCount);
        res = ArrayOf(outType, dims, ptr);
        TFlipUD2dReal<logical>(
            (logical*)arrayIn.getDataPointer(), ptr, dims.getRows(), dims.getColumns());
    } break;
    case NLS_UINT8: {
        needToOverload = false;
        uint8* ptr = (uint8*)ArrayOf::allocateArrayOf(outType, elementCount);
        res = ArrayOf(outType, dims, ptr);
        TFlipUD2dReal<uint8>(
            (uint8*)arrayIn.getDataPointer(), ptr, dims.getRows(), dims.getColumns());
    } break;
    case NLS_INT8: {
        needToOverload = false;
        int8* ptr = (int8*)ArrayOf::allocateArrayOf(outType, elementCount);
        res = ArrayOf(outType, dims, ptr);
        TFlipUD2dReal<int8>(
            (int8*)arrayIn.getDataPointer(), ptr, dims.getRows(), dims.getColumns());
    } break;
    case NLS_UINT16: {
        needToOverload = false;
        uint16* ptr = (uint16*)ArrayOf::allocateArrayOf(outType, elementCount);
        res = ArrayOf(outType, dims, ptr);
        TFlipUD2dReal<uint16>(
            (uint16*)arrayIn.getDataPointer(), ptr, dims.getRows(), dims.getColumns());
    } break;
    case NLS_INT16: {
        needToOverload = false;
        int16* ptr = (int16*)ArrayOf::allocateArrayOf(outType, elementCount);
        res = ArrayOf(outType, dims, ptr);
        TFlipUD2dReal<int16>(
            (int16*)arrayIn.getDataPointer(), ptr, dims.getRows(), dims.getColumns());
    } break;
    case NLS_UINT32: {
        needToOverload = false;
        uint32* ptr = (uint32*)ArrayOf::allocateArrayOf(outType, elementCount);
        res = ArrayOf(outType, dims, ptr);
        TFlipUD2dReal<uint32>(
            (uint32*)arrayIn.getDataPointer(), ptr, dims.getRows(), dims.getColumns());
    } break;
    case NLS_INT32: {
        needToOverload = false;
        int32* ptr = (int32*)ArrayOf::allocateArrayOf(outType, elementCount);
        res = ArrayOf(outType, dims, ptr);
        TFlipUD2dReal<int32>(
            (int32*)arrayIn.getDataPointer(), ptr, dims.getRows(), dims.getColumns());
    } break;
    case NLS_UINT64: {
        needToOverload = false;
        uint64* ptr = (uint64*)ArrayOf::allocateArrayOf(outType, elementCount);
        res = ArrayOf(outType, dims, ptr);
        TFlipUD2dReal<uint64>(
            (uint64*)arrayIn.getDataPointer(), ptr, dims.getRows(), dims.getColumns());
    } break;
    case NLS_INT64: {
        needToOverload = false;
        int64* ptr = (int64*)ArrayOf::allocateArrayOf(outType, elementCount);
        res = ArrayOf(outType, dims, ptr);
        TFlipUD2dReal<int64>(
            (int64*)arrayIn.getDataPointer(), ptr, dims.getRows(), dims.getColumns());
    } break;
    case NLS_SINGLE: {
        needToOverload = false;
        single* ptr = (single*)ArrayOf::allocateArrayOf(outType, elementCount);
        res = ArrayOf(outType, dims, ptr);
        TFlipUD2dReal<single>(
            (single*)arrayIn.getDataPointer(), ptr, dims.getRows(), dims.getColumns());
    } break;
    case NLS_DOUBLE: {
        needToOverload = false;
        double* ptr = (double*)ArrayOf::allocateArrayOf(outType, elementCount);
        res = ArrayOf(outType, dims, ptr);
        TFlipUD2dReal<double>(
            (double*)arrayIn.getDataPointer(), ptr, dims.getRows(), dims.getColumns());
    } break;
    case NLS_SCOMPLEX: {
        needToOverload = false;
        single* ptr = (single*)ArrayOf::allocateArrayOf(outType, elementCount);
        res = ArrayOf(outType, dims, ptr);
        TFlipUD2dComplex<single>(
            (single*)arrayIn.getDataPointer(), ptr, dims.getRows(), dims.getColumns());
    } break;
    case NLS_DCOMPLEX: {
        needToOverload = false;
        double* ptr = (double*)ArrayOf::allocateArrayOf(outType, elementCount);
        res = ArrayOf(outType, dims, ptr);
        TFlipUD2dComplex<double>(
            (double*)arrayIn.getDataPointer(), ptr, dims.getRows(), dims.getColumns());
    } break;
    case NLS_CHAR: {
        needToOverload = false;
        charType* ptr = (charType*)ArrayOf::allocateArrayOf(outType, elementCount);
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
