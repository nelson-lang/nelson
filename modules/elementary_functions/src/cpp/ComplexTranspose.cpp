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
#include "ComplexTranspose.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
void
complexTransposeRealTemplate(Dimensions dimsA, T* ptrA, T* ptrRes)
{
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matOrigin(
        (T*)ptrA, dimsA.getRows(), dimsA.getColumns());
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matTransposed(
        (T*)ptrRes, dimsA.getColumns(), dimsA.getRows());
    matTransposed = matOrigin.conjugate().transpose().eval();
}
//=============================================================================
template <class T>
void
complexTransposeComplexTemplate(Dimensions dimsA, T* ptrA, T* ptrRes)
{
    std::complex<T>* matCplxA = reinterpret_cast<std::complex<T>*>(ptrA);
    std::complex<T>* matCplxRes = reinterpret_cast<std::complex<T>*>(ptrRes);
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matOrigin(
        matCplxA, dimsA.getRows(), dimsA.getColumns());
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matTransposed(
        matCplxRes, dimsA.getColumns(), dimsA.getRows());
    matTransposed = matOrigin.conjugate().transpose().eval();
}
//=============================================================================
ArrayOf
ComplexTranspose(const ArrayOf& A, bool& needToOverload)
{
    /* Code not factorized with Transpose to speed up at runtime */
    needToOverload = false;
    Class classA = A.getDataClass();
    if ((classA < NLS_LOGICAL || A.isSparse())
        && !(A.isCell() || A.isStruct() || A.isStringArray())) {
        needToOverload = true;
        return ArrayOf();
    }
    Dimensions dimsA = A.getDimensions();
    bool isSupported = (A.isEmpty() || A.isScalar() || A.is2D());
    if (!isSupported) {
        std::wstring msg = _W("ctranspose on N-D array is undefined.");
        Error(msg);
    }
    ArrayOf Res(A);
    Res.ensureSingleOwner();
    Dimensions dimsRes(dimsA.getColumns(), dimsA.getRows());
    if (A.isEmpty()) {
        Res.reshape(dimsRes);
        return Res;
    }
    switch (classA) {
    case NLS_LOGICAL: {
        complexTransposeRealTemplate<logical>(
            dimsA, (logical*)A.getDataPointer(), (logical*)Res.getDataPointer());
    } break;
    case NLS_UINT8: {
        complexTransposeRealTemplate<uint8>(
            dimsA, (uint8*)A.getDataPointer(), (uint8*)Res.getDataPointer());
    } break;
    case NLS_INT8: {
        complexTransposeRealTemplate<int8>(
            dimsA, (int8*)A.getDataPointer(), (int8*)Res.getDataPointer());
    } break;
    case NLS_UINT16: {
        complexTransposeRealTemplate<uint16>(
            dimsA, (uint16*)A.getDataPointer(), (uint16*)Res.getDataPointer());
    } break;
    case NLS_INT16: {
        complexTransposeRealTemplate<int16>(
            dimsA, (int16*)A.getDataPointer(), (int16*)Res.getDataPointer());
    } break;
    case NLS_UINT32: {
        complexTransposeRealTemplate<uint32>(
            dimsA, (uint32*)A.getDataPointer(), (uint32*)Res.getDataPointer());
    } break;
    case NLS_INT32: {
        complexTransposeRealTemplate<int32>(
            dimsA, (int32*)A.getDataPointer(), (int32*)Res.getDataPointer());
    } break;
    case NLS_UINT64: {
        complexTransposeRealTemplate<uint64>(
            dimsA, (uint64*)A.getDataPointer(), (uint64*)Res.getDataPointer());
    } break;
    case NLS_INT64: {
        complexTransposeRealTemplate<int64>(
            dimsA, (int64*)A.getDataPointer(), (int64*)Res.getDataPointer());
    } break;
    case NLS_SINGLE: {
        complexTransposeRealTemplate<single>(
            dimsA, (single*)A.getDataPointer(), (single*)Res.getDataPointer());
    } break;
    case NLS_DOUBLE: {
        complexTransposeRealTemplate<double>(
            dimsA, (double*)A.getDataPointer(), (double*)Res.getDataPointer());
    } break;
    case NLS_SCOMPLEX: {
        complexTransposeComplexTemplate<single>(
            dimsA, (single*)A.getDataPointer(), (single*)Res.getDataPointer());
    } break;
    case NLS_DCOMPLEX: {
        complexTransposeComplexTemplate<double>(
            dimsA, (double*)A.getDataPointer(), (double*)Res.getDataPointer());
    } break;
    case NLS_CHAR: {
        complexTransposeRealTemplate<charType>(
            dimsA, (charType*)A.getDataPointer(), (charType*)Res.getDataPointer());
    } break;
    case NLS_STRUCT_ARRAY:
    case NLS_STRING_ARRAY:
    case NLS_CELL_ARRAY: {
        void* destPtr = (void*)Res.getDataPointer();
        indexType rowCount = dimsA.getRows();
        indexType colCount = dimsA.getColumns();
        ArrayOf RR = A;
        int ptr;
        ptr = 0;
        for (indexType i = 0; i < rowCount; i++) {
            for (indexType j = 0; j < colCount; j++) {
                RR.copyElements(i + j * rowCount, destPtr, ptr, 1);
                ptr++;
            }
        }
    } break;
    default: {
        needToOverload = true;
        return ArrayOf();
    } break;
    }
    Res.reshape(dimsRes);
    return Res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
