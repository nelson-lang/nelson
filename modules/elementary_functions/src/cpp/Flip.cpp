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
//=============================================================================
namespace Nelson {
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
        Eigen::Map<Eigen::Matrix<logical, Eigen::Dynamic, Eigen::Dynamic>> matIn(
            (logical*)arrayIn.getDataPointer(), dims.getRows(), dims.getColumns());
        Eigen::Map<Eigen::Matrix<logical, Eigen::Dynamic, Eigen::Dynamic>> matOut(
            ptr, dims.getRows(), dims.getColumns());
        matOut = matIn.rowwise().reverse().eval();
    } break;
    case NLS_UINT8: {
        needToOverload = false;
        uint8* ptr = (uint8*)ArrayOf::allocateArrayOf(outType, elementCount);
        res = ArrayOf(outType, dims, ptr);
        Eigen::Map<Eigen::Matrix<uint8, Eigen::Dynamic, Eigen::Dynamic>> matIn(
            (uint8*)arrayIn.getDataPointer(), dims.getRows(), dims.getColumns());
        Eigen::Map<Eigen::Matrix<uint8, Eigen::Dynamic, Eigen::Dynamic>> matOut(
            ptr, dims.getRows(), dims.getColumns());
        matOut = matIn.rowwise().reverse().eval();
    } break;
    case NLS_INT8: {
        needToOverload = false;
        int8* ptr = (int8*)ArrayOf::allocateArrayOf(outType, elementCount);
        res = ArrayOf(outType, dims, ptr);
        Eigen::Map<Eigen::Matrix<int8, Eigen::Dynamic, Eigen::Dynamic>> matIn(
            (int8*)arrayIn.getDataPointer(), dims.getRows(), dims.getColumns());
        Eigen::Map<Eigen::Matrix<int8, Eigen::Dynamic, Eigen::Dynamic>> matOut(
            ptr, dims.getRows(), dims.getColumns());
        matOut = matIn.rowwise().reverse().eval();
    } break;
    case NLS_UINT16: {
        needToOverload = false;
        uint16* ptr = (uint16*)ArrayOf::allocateArrayOf(outType, elementCount);
        res = ArrayOf(outType, dims, ptr);
        Eigen::Map<Eigen::Matrix<uint16, Eigen::Dynamic, Eigen::Dynamic>> matIn(
            (uint16*)arrayIn.getDataPointer(), dims.getRows(), dims.getColumns());
        Eigen::Map<Eigen::Matrix<uint16, Eigen::Dynamic, Eigen::Dynamic>> matOut(
            ptr, dims.getRows(), dims.getColumns());
        matOut = matIn.rowwise().reverse().eval();
    } break;
    case NLS_INT16: {
        needToOverload = false;
        int16* ptr = (int16*)ArrayOf::allocateArrayOf(outType, elementCount);
        res = ArrayOf(outType, dims, ptr);
        Eigen::Map<Eigen::Matrix<int16, Eigen::Dynamic, Eigen::Dynamic>> matIn(
            (int16*)arrayIn.getDataPointer(), dims.getRows(), dims.getColumns());
        Eigen::Map<Eigen::Matrix<int16, Eigen::Dynamic, Eigen::Dynamic>> matOut(
            ptr, dims.getRows(), dims.getColumns());
        matOut = matIn.rowwise().reverse().eval();
    } break;
    case NLS_UINT32: {
        needToOverload = false;
        uint32* ptr = (uint32*)ArrayOf::allocateArrayOf(outType, elementCount);
        res = ArrayOf(outType, dims, ptr);
        Eigen::Map<Eigen::Matrix<uint32, Eigen::Dynamic, Eigen::Dynamic>> matIn(
            (uint32*)arrayIn.getDataPointer(), dims.getRows(), dims.getColumns());
        Eigen::Map<Eigen::Matrix<uint32, Eigen::Dynamic, Eigen::Dynamic>> matOut(
            ptr, dims.getRows(), dims.getColumns());
        matOut = matIn.rowwise().reverse().eval();
    } break;
    case NLS_INT32: {
        needToOverload = false;
        int32* ptr = (int32*)ArrayOf::allocateArrayOf(outType, elementCount);
        res = ArrayOf(outType, dims, ptr);
        Eigen::Map<Eigen::Matrix<int32, Eigen::Dynamic, Eigen::Dynamic>> matIn(
            (int32*)arrayIn.getDataPointer(), dims.getRows(), dims.getColumns());
        Eigen::Map<Eigen::Matrix<int32, Eigen::Dynamic, Eigen::Dynamic>> matOut(
            ptr, dims.getRows(), dims.getColumns());
        matOut = matIn.rowwise().reverse().eval();
    } break;
    case NLS_UINT64: {
        needToOverload = false;
        uint64* ptr = (uint64*)ArrayOf::allocateArrayOf(outType, elementCount);
        res = ArrayOf(outType, dims, ptr);
        Eigen::Map<Eigen::Matrix<uint64, Eigen::Dynamic, Eigen::Dynamic>> matIn(
            (uint64*)arrayIn.getDataPointer(), dims.getRows(), dims.getColumns());
        Eigen::Map<Eigen::Matrix<uint64, Eigen::Dynamic, Eigen::Dynamic>> matOut(
            ptr, dims.getRows(), dims.getColumns());
        matOut = matIn.rowwise().reverse().eval();
    } break;
    case NLS_INT64: {
        needToOverload = false;
        int64* ptr = (int64*)ArrayOf::allocateArrayOf(outType, elementCount);
        res = ArrayOf(outType, dims, ptr);
        Eigen::Map<Eigen::Matrix<int64, Eigen::Dynamic, Eigen::Dynamic>> matIn(
            (int64*)arrayIn.getDataPointer(), dims.getRows(), dims.getColumns());
        Eigen::Map<Eigen::Matrix<int64, Eigen::Dynamic, Eigen::Dynamic>> matOut(
            ptr, dims.getRows(), dims.getColumns());
        matOut = matIn.rowwise().reverse().eval();
    } break;
    case NLS_SINGLE: {
        needToOverload = false;
        single* ptr = (single*)ArrayOf::allocateArrayOf(outType, elementCount);
        res = ArrayOf(outType, dims, ptr);
        Eigen::Map<Eigen::Matrix<single, Eigen::Dynamic, Eigen::Dynamic>> matIn(
            (single*)arrayIn.getDataPointer(), dims.getRows(), dims.getColumns());
        Eigen::Map<Eigen::Matrix<single, Eigen::Dynamic, Eigen::Dynamic>> matOut(
            ptr, dims.getRows(), dims.getColumns());
        matOut = matIn.rowwise().reverse().eval();
    } break;
    case NLS_DOUBLE: {
        needToOverload = false;
        double* ptr = (double*)ArrayOf::allocateArrayOf(outType, elementCount);
        res = ArrayOf(outType, dims, ptr);
        Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic>> matIn(
            (double*)arrayIn.getDataPointer(), dims.getRows(), dims.getColumns());
        Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic>> matOut(
            ptr, dims.getRows(), dims.getColumns());
        matOut = matIn.rowwise().reverse().eval();
    } break;
    case NLS_SCOMPLEX: {
        needToOverload = false;
        single* ptr = (single*)ArrayOf::allocateArrayOf(outType, elementCount);
        res = ArrayOf(outType, dims, ptr);
        auto* Az = reinterpret_cast<singlecomplex*>((single*)arrayIn.getDataPointer());
        Eigen::Map<Eigen::Matrix<std::complex<single>, Eigen::Dynamic, Eigen::Dynamic>> matIn(
            Az, dims.getRows(), dims.getColumns());
        auto* Cz = reinterpret_cast<singlecomplex*>(ptr);
        Eigen::Map<Eigen::Matrix<std::complex<single>, Eigen::Dynamic, Eigen::Dynamic>> matOut(
            Cz, dims.getRows(), dims.getColumns());
        matOut = matIn.rowwise().reverse().eval();
    } break;
    case NLS_DCOMPLEX: {
        needToOverload = false;
        double* ptr = (double*)ArrayOf::allocateArrayOf(outType, elementCount);
        res = ArrayOf(outType, dims, ptr);
        auto* Az = reinterpret_cast<doublecomplex*>((single*)arrayIn.getDataPointer());
        Eigen::Map<Eigen::Matrix<std::complex<double>, Eigen::Dynamic, Eigen::Dynamic>> matIn(
            Az, dims.getRows(), dims.getColumns());
        auto* Cz = reinterpret_cast<doublecomplex*>(ptr);
        Eigen::Map<Eigen::Matrix<std::complex<double>, Eigen::Dynamic, Eigen::Dynamic>> matOut(
            Cz, dims.getRows(), dims.getColumns());
        matOut = matIn.rowwise().reverse().eval();
    } break;
    case NLS_CHAR: {
        needToOverload = false;
        charType* ptr = (charType*)ArrayOf::allocateArrayOf(outType, elementCount);
        res = ArrayOf(outType, dims, ptr);
        Eigen::Map<Eigen::Matrix<charType, Eigen::Dynamic, Eigen::Dynamic>> matIn(
            (charType*)arrayIn.getDataPointer(), dims.getRows(), dims.getColumns());
        Eigen::Map<Eigen::Matrix<charType, Eigen::Dynamic, Eigen::Dynamic>> matOut(
            ptr, dims.getRows(), dims.getColumns());
        matOut = matIn.rowwise().reverse().eval();
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
        Eigen::Map<Eigen::Matrix<logical, Eigen::Dynamic, Eigen::Dynamic>> matIn(
            (logical*)arrayIn.getDataPointer(), dims.getRows(), dims.getColumns());
        Eigen::Map<Eigen::Matrix<logical, Eigen::Dynamic, Eigen::Dynamic>> matOut(
            ptr, dims.getRows(), dims.getColumns());
        matOut = matIn.colwise().reverse().eval();
    } break;
    case NLS_UINT8: {
        needToOverload = false;
        uint8* ptr = (uint8*)ArrayOf::allocateArrayOf(outType, elementCount);
        res = ArrayOf(outType, dims, ptr);
        Eigen::Map<Eigen::Matrix<uint8, Eigen::Dynamic, Eigen::Dynamic>> matIn(
            (uint8*)arrayIn.getDataPointer(), dims.getRows(), dims.getColumns());
        Eigen::Map<Eigen::Matrix<uint8, Eigen::Dynamic, Eigen::Dynamic>> matOut(
            ptr, dims.getRows(), dims.getColumns());
        matOut = matIn.colwise().reverse().eval();
    } break;
    case NLS_INT8: {
        needToOverload = false;
        int8* ptr = (int8*)ArrayOf::allocateArrayOf(outType, elementCount);
        res = ArrayOf(outType, dims, ptr);
        Eigen::Map<Eigen::Matrix<int8, Eigen::Dynamic, Eigen::Dynamic>> matIn(
            (int8*)arrayIn.getDataPointer(), dims.getRows(), dims.getColumns());
        Eigen::Map<Eigen::Matrix<int8, Eigen::Dynamic, Eigen::Dynamic>> matOut(
            ptr, dims.getRows(), dims.getColumns());
        matOut = matIn.colwise().reverse().eval();
    } break;
    case NLS_UINT16: {
        needToOverload = false;
        uint16* ptr = (uint16*)ArrayOf::allocateArrayOf(outType, elementCount);
        res = ArrayOf(outType, dims, ptr);
        Eigen::Map<Eigen::Matrix<uint16, Eigen::Dynamic, Eigen::Dynamic>> matIn(
            (uint16*)arrayIn.getDataPointer(), dims.getRows(), dims.getColumns());
        Eigen::Map<Eigen::Matrix<uint16, Eigen::Dynamic, Eigen::Dynamic>> matOut(
            ptr, dims.getRows(), dims.getColumns());
        matOut = matIn.colwise().reverse().eval();
    } break;
    case NLS_INT16: {
        needToOverload = false;
        int16* ptr = (int16*)ArrayOf::allocateArrayOf(outType, elementCount);
        res = ArrayOf(outType, dims, ptr);
        Eigen::Map<Eigen::Matrix<int16, Eigen::Dynamic, Eigen::Dynamic>> matIn(
            (int16*)arrayIn.getDataPointer(), dims.getRows(), dims.getColumns());
        Eigen::Map<Eigen::Matrix<int16, Eigen::Dynamic, Eigen::Dynamic>> matOut(
            ptr, dims.getRows(), dims.getColumns());
        matOut = matIn.colwise().reverse().eval();
    } break;
    case NLS_UINT32: {
        needToOverload = false;
        uint32* ptr = (uint32*)ArrayOf::allocateArrayOf(outType, elementCount);
        res = ArrayOf(outType, dims, ptr);
        Eigen::Map<Eigen::Matrix<uint32, Eigen::Dynamic, Eigen::Dynamic>> matIn(
            (uint32*)arrayIn.getDataPointer(), dims.getRows(), dims.getColumns());
        Eigen::Map<Eigen::Matrix<uint32, Eigen::Dynamic, Eigen::Dynamic>> matOut(
            ptr, dims.getRows(), dims.getColumns());
        matOut = matIn.colwise().reverse().eval();
    } break;
    case NLS_INT32: {
        needToOverload = false;
        int32* ptr = (int32*)ArrayOf::allocateArrayOf(outType, elementCount);
        res = ArrayOf(outType, dims, ptr);
        Eigen::Map<Eigen::Matrix<int32, Eigen::Dynamic, Eigen::Dynamic>> matIn(
            (int32*)arrayIn.getDataPointer(), dims.getRows(), dims.getColumns());
        Eigen::Map<Eigen::Matrix<int32, Eigen::Dynamic, Eigen::Dynamic>> matOut(
            ptr, dims.getRows(), dims.getColumns());
        matOut = matIn.colwise().reverse().eval();
    } break;
    case NLS_UINT64: {
        needToOverload = false;
        uint64* ptr = (uint64*)ArrayOf::allocateArrayOf(outType, elementCount);
        res = ArrayOf(outType, dims, ptr);
        Eigen::Map<Eigen::Matrix<uint64, Eigen::Dynamic, Eigen::Dynamic>> matIn(
            (uint64*)arrayIn.getDataPointer(), dims.getRows(), dims.getColumns());
        Eigen::Map<Eigen::Matrix<uint64, Eigen::Dynamic, Eigen::Dynamic>> matOut(
            ptr, dims.getRows(), dims.getColumns());
        matOut = matIn.colwise().reverse().eval();
    } break;
    case NLS_INT64: {
        needToOverload = false;
        int64* ptr = (int64*)ArrayOf::allocateArrayOf(outType, elementCount);
        res = ArrayOf(outType, dims, ptr);
        Eigen::Map<Eigen::Matrix<int64, Eigen::Dynamic, Eigen::Dynamic>> matIn(
            (int64*)arrayIn.getDataPointer(), dims.getRows(), dims.getColumns());
        Eigen::Map<Eigen::Matrix<int64, Eigen::Dynamic, Eigen::Dynamic>> matOut(
            ptr, dims.getRows(), dims.getColumns());
        matOut = matIn.colwise().reverse().eval();
    } break;
    case NLS_SINGLE: {
        needToOverload = false;
        single* ptr = (single*)ArrayOf::allocateArrayOf(outType, elementCount);
        res = ArrayOf(outType, dims, ptr);
        Eigen::Map<Eigen::Matrix<single, Eigen::Dynamic, Eigen::Dynamic>> matIn(
            (single*)arrayIn.getDataPointer(), dims.getRows(), dims.getColumns());
        Eigen::Map<Eigen::Matrix<single, Eigen::Dynamic, Eigen::Dynamic>> matOut(
            ptr, dims.getRows(), dims.getColumns());
        matOut = matIn.colwise().reverse().eval();
    } break;
    case NLS_DOUBLE: {
        needToOverload = false;
        double* ptr = (double*)ArrayOf::allocateArrayOf(outType, elementCount);
        res = ArrayOf(outType, dims, ptr);
        Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic>> matIn(
            (double*)arrayIn.getDataPointer(), dims.getRows(), dims.getColumns());
        Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic>> matOut(
            ptr, dims.getRows(), dims.getColumns());
        matOut = matIn.colwise().reverse().eval();
    } break;
    case NLS_SCOMPLEX: {
        needToOverload = false;
        single* ptr = (single*)ArrayOf::allocateArrayOf(outType, elementCount);
        res = ArrayOf(outType, dims, ptr);
        auto* Az = reinterpret_cast<singlecomplex*>((single*)arrayIn.getDataPointer());
        Eigen::Map<Eigen::Matrix<std::complex<single>, Eigen::Dynamic, Eigen::Dynamic>> matIn(
            Az, dims.getRows(), dims.getColumns());
        auto* Cz = reinterpret_cast<singlecomplex*>(ptr);
        Eigen::Map<Eigen::Matrix<std::complex<single>, Eigen::Dynamic, Eigen::Dynamic>> matOut(
            Cz, dims.getRows(), dims.getColumns());
        matOut = matIn.colwise().reverse().eval();
    } break;
    case NLS_DCOMPLEX: {
        needToOverload = false;
        double* ptr = (double*)ArrayOf::allocateArrayOf(outType, elementCount);
        res = ArrayOf(outType, dims, ptr);
        auto* Az = reinterpret_cast<doublecomplex*>((single*)arrayIn.getDataPointer());
        Eigen::Map<Eigen::Matrix<std::complex<double>, Eigen::Dynamic, Eigen::Dynamic>> matIn(
            Az, dims.getRows(), dims.getColumns());
        auto* Cz = reinterpret_cast<doublecomplex*>(ptr);
        Eigen::Map<Eigen::Matrix<std::complex<double>, Eigen::Dynamic, Eigen::Dynamic>> matOut(
            Cz, dims.getRows(), dims.getColumns());
        matOut = matIn.colwise().reverse().eval();
    } break;
    case NLS_CHAR: {
        needToOverload = false;
        charType* ptr = (charType*)ArrayOf::allocateArrayOf(outType, elementCount);
        res = ArrayOf(outType, dims, ptr);
        Eigen::Map<Eigen::Matrix<charType, Eigen::Dynamic, Eigen::Dynamic>> matIn(
            (charType*)arrayIn.getDataPointer(), dims.getRows(), dims.getColumns());
        Eigen::Map<Eigen::Matrix<charType, Eigen::Dynamic, Eigen::Dynamic>> matOut(
            ptr, dims.getRows(), dims.getColumns());
        matOut = matIn.colwise().reverse().eval();
    } break;
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
