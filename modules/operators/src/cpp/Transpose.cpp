//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "Transpose.hpp"
#include "TransposeSparseDouble.hpp"
#include "TransposeSparseLogical.hpp"
#include "ClassName.hpp"
#include "i18n.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
void
transposeRealTemplate(const Dimensions& dimsA, T* ptrA, T* ptrRes)
{
    ompIndexType nbRows = dimsA.getRows();
    ompIndexType nbColumns = dimsA.getColumns();
    OMP_PARALLEL_FOR_LOOP(nbRows * nbColumns)
    for (ompIndexType index = 0; index < nbRows * nbColumns; index++) {
        ompIndexType i = index % nbRows;
        ompIndexType j = index / nbRows;
        ptrRes[i * nbColumns + j] = ptrA[j * nbRows + i];
    }
}
//=============================================================================
template <class T>
void
transposeComplexTemplate(const Dimensions& dimsA, T* ptrA, T* ptrRes)
{
    auto* matCplxA = reinterpret_cast<std::complex<T>*>(ptrA);
    auto* matCplxRes = reinterpret_cast<std::complex<T>*>(ptrRes);
    ompIndexType nbRows = dimsA.getRows();
    ompIndexType nbColumns = dimsA.getColumns();
    OMP_PARALLEL_FOR_LOOP(nbRows * nbColumns)
    for (ompIndexType index = 0; index < nbRows * nbColumns; index++) {
        ompIndexType i = index % nbRows;
        ompIndexType j = index / nbRows;
        matCplxRes[i * nbColumns + j] = matCplxA[j * nbRows + i];
    }
}
//=============================================================================
ArrayOf
Transpose(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    NelsonType classA = A.getDataClass();
    if ((classA > NLS_CHAR) && !(A.isCell() || A.isStruct() || A.isStringArray())) {
        needToOverload = true;
        return {};
    }
    Dimensions dimsA = A.getDimensions();
    bool isSupported = (A.isEmpty() || A.isScalar() || A.is2D());
    if (!isSupported) {
        std::wstring msg = _W("transpose on N-D array is undefined.");
        Error(msg);
    }
    ArrayOf Res;
    Dimensions dimsRes(dimsA.getColumns(), dimsA.getRows());
    if (A.isEmpty()) {
        Res = A;
        Res.ensureSingleOwner();
        Res.reshape(dimsRes);
        return Res;
    }
    switch (classA) {
    case NLS_LOGICAL: {
        if (A.isSparse()) {
            return TransposeSparseLogical(A);
        }
        logical* ptrRes = (logical*)ArrayOf::allocateArrayOf(classA, dimsRes.getElementCount());
        Res = ArrayOf(classA, dimsRes, ptrRes);
        transposeRealTemplate<logical>(
            dimsA, (logical*)A.getDataPointer(), (logical*)Res.getDataPointer());
    } break;
    case NLS_UINT8: {
        uint8* ptrRes = (uint8*)ArrayOf::allocateArrayOf(classA, dimsRes.getElementCount());
        Res = ArrayOf(classA, dimsRes, ptrRes);
        transposeRealTemplate<uint8>(
            dimsA, (uint8*)A.getDataPointer(), (uint8*)Res.getDataPointer());
    } break;
    case NLS_INT8: {
        int8* ptrRes = (int8*)ArrayOf::allocateArrayOf(classA, dimsRes.getElementCount());
        Res = ArrayOf(classA, dimsRes, ptrRes);
        transposeRealTemplate<int8>(dimsA, (int8*)A.getDataPointer(), (int8*)Res.getDataPointer());
    } break;
    case NLS_UINT16: {
        uint16* ptrRes = (uint16*)ArrayOf::allocateArrayOf(classA, dimsRes.getElementCount());
        Res = ArrayOf(classA, dimsRes, ptrRes);
        transposeRealTemplate<uint16>(
            dimsA, (uint16*)A.getDataPointer(), (uint16*)Res.getDataPointer());
    } break;
    case NLS_INT16: {
        int16* ptrRes = (int16*)ArrayOf::allocateArrayOf(classA, dimsRes.getElementCount());
        Res = ArrayOf(classA, dimsRes, ptrRes);
        transposeRealTemplate<int16>(
            dimsA, (int16*)A.getDataPointer(), (int16*)Res.getDataPointer());
    } break;
    case NLS_UINT32: {
        uint32* ptrRes = (uint32*)ArrayOf::allocateArrayOf(classA, dimsRes.getElementCount());
        Res = ArrayOf(classA, dimsRes, ptrRes);
        transposeRealTemplate<uint32>(
            dimsA, (uint32*)A.getDataPointer(), (uint32*)Res.getDataPointer());
    } break;
    case NLS_INT32: {
        int32* ptrRes = (int32*)ArrayOf::allocateArrayOf(classA, dimsRes.getElementCount());
        Res = ArrayOf(classA, dimsRes, ptrRes);
        transposeRealTemplate<int32>(
            dimsA, (int32*)A.getDataPointer(), (int32*)Res.getDataPointer());
    } break;
    case NLS_UINT64: {
        uint64* ptrRes = (uint64*)ArrayOf::allocateArrayOf(classA, dimsRes.getElementCount());
        Res = ArrayOf(classA, dimsRes, ptrRes);
        transposeRealTemplate<uint64>(
            dimsA, (uint64*)A.getDataPointer(), (uint64*)Res.getDataPointer());
    } break;
    case NLS_INT64: {
        int64* ptrRes = (int64*)ArrayOf::allocateArrayOf(classA, dimsRes.getElementCount());
        Res = ArrayOf(classA, dimsRes, ptrRes);
        transposeRealTemplate<int64>(
            dimsA, (int64*)A.getDataPointer(), (int64*)Res.getDataPointer());
    } break;
    case NLS_SINGLE: {
        single* ptrRes = (single*)ArrayOf::allocateArrayOf(classA, dimsRes.getElementCount());
        Res = ArrayOf(classA, dimsRes, ptrRes);
        transposeRealTemplate<single>(
            dimsA, (single*)A.getDataPointer(), (single*)Res.getDataPointer());
    } break;
    case NLS_DOUBLE: {
        if (A.isSparse()) {
            return TransposeSparseDouble(A);
        }
        double* ptrRes = (double*)ArrayOf::allocateArrayOf(classA, dimsRes.getElementCount());
        Res = ArrayOf(classA, dimsRes, ptrRes);
        transposeRealTemplate<double>(
            dimsA, (double*)A.getDataPointer(), (double*)Res.getDataPointer());
    } break;
    case NLS_SCOMPLEX: {
        single* ptrRes = (single*)ArrayOf::allocateArrayOf(classA, dimsRes.getElementCount() * 2);
        Res = ArrayOf(classA, dimsRes, ptrRes);
        transposeComplexTemplate<single>(
            dimsA, (single*)A.getDataPointer(), (single*)Res.getDataPointer());
    } break;
    case NLS_DCOMPLEX: {
        if (A.isSparse()) {
            return TransposeSparseDouble(A);
        }
        double* ptrRes = (double*)ArrayOf::allocateArrayOf(classA, dimsRes.getElementCount() * 2);
        Res = ArrayOf(classA, dimsRes, ptrRes);
        transposeComplexTemplate<double>(
            dimsA, (double*)A.getDataPointer(), (double*)Res.getDataPointer());
    } break;
    case NLS_CHAR: {
        charType* ptrRes = (charType*)ArrayOf::allocateArrayOf(classA, dimsRes.getElementCount());
        Res = ArrayOf(classA, dimsRes, ptrRes);
        transposeRealTemplate<charType>(
            dimsA, (charType*)A.getDataPointer(), (charType*)Res.getDataPointer());
    } break;
    case NLS_CLASS_ARRAY:
    case NLS_FUNCTION_HANDLE:
    case NLS_STRUCT_ARRAY:
    case NLS_STRING_ARRAY:
    case NLS_CELL_ARRAY: {
        Res = A;
        Res.ensureSingleOwner();
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
        return {};
    } break;
    }
    Res.reshape(dimsRes);
    return Res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
