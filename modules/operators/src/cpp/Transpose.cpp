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
#include "Transpose.hpp"
#include "TransposeSparseDouble.hpp"
#include "TransposeSparseLogical.hpp"
#include "ClassName.hpp"
#include "i18n.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
void
transposeRealTemplate(const Dimensions& dimsA, T* ptrA, T* ptrRes)
{
    ompIndexType nbRows = dimsA.rows();
    ompIndexType nbColumns = dimsA.columns();
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
    ompIndexType nbRows = dimsA.rows();
    ompIndexType nbColumns = dimsA.columns();
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
    NelsonType classA = A.dataClass();
    if ((classA > NLS_CHAR)
        && !(A.isCell() || A.isStruct() || A.isStringArray() || A.isClassType() || A.isHandle())) {
        needToOverload = true;
        return {};
    }
    Dimensions dimsA = A.dimensions();
    bool isSupported = (A.isEmpty() || A.isScalar() || A.is2D());
    if (!isSupported) {
        std::wstring msg = resolveMessageTemplate(
            L"Nelson:operators:transposeNDArray", L"transpose on N-D array is undefined.");
        Error(L"Nelson:operators:invalidDimensions", msg);
    }
    ArrayOf Res;
    Dimensions dimsRes(dimsA.columns(), dimsA.rows());
    if (A.isEmpty() && !A.isClassType()) {
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
        logical* ptrRes = (logical*)ArrayOf::allocateArrayOf(classA, dimsRes.elementCount());
        Res = ArrayOf(classA, dimsRes, ptrRes);
        transposeRealTemplate<logical>(
            dimsA, (logical*)A.dataPointer(), (logical*)Res.dataPointer());
    } break;
    case NLS_UINT8: {
        uint8* ptrRes = (uint8*)ArrayOf::allocateArrayOf(classA, dimsRes.elementCount());
        Res = ArrayOf(classA, dimsRes, ptrRes);
        transposeRealTemplate<uint8>(dimsA, (uint8*)A.dataPointer(), (uint8*)Res.dataPointer());
    } break;
    case NLS_INT8: {
        int8* ptrRes = (int8*)ArrayOf::allocateArrayOf(classA, dimsRes.elementCount());
        Res = ArrayOf(classA, dimsRes, ptrRes);
        transposeRealTemplate<int8>(dimsA, (int8*)A.dataPointer(), (int8*)Res.dataPointer());
    } break;
    case NLS_UINT16: {
        uint16* ptrRes = (uint16*)ArrayOf::allocateArrayOf(classA, dimsRes.elementCount());
        Res = ArrayOf(classA, dimsRes, ptrRes);
        transposeRealTemplate<uint16>(dimsA, (uint16*)A.dataPointer(), (uint16*)Res.dataPointer());
    } break;
    case NLS_INT16: {
        int16* ptrRes = (int16*)ArrayOf::allocateArrayOf(classA, dimsRes.elementCount());
        Res = ArrayOf(classA, dimsRes, ptrRes);
        transposeRealTemplate<int16>(dimsA, (int16*)A.dataPointer(), (int16*)Res.dataPointer());
    } break;
    case NLS_UINT32: {
        uint32* ptrRes = (uint32*)ArrayOf::allocateArrayOf(classA, dimsRes.elementCount());
        Res = ArrayOf(classA, dimsRes, ptrRes);
        transposeRealTemplate<uint32>(dimsA, (uint32*)A.dataPointer(), (uint32*)Res.dataPointer());
    } break;
    case NLS_INT32: {
        int32* ptrRes = (int32*)ArrayOf::allocateArrayOf(classA, dimsRes.elementCount());
        Res = ArrayOf(classA, dimsRes, ptrRes);
        transposeRealTemplate<int32>(dimsA, (int32*)A.dataPointer(), (int32*)Res.dataPointer());
    } break;
    case NLS_UINT64: {
        uint64* ptrRes = (uint64*)ArrayOf::allocateArrayOf(classA, dimsRes.elementCount());
        Res = ArrayOf(classA, dimsRes, ptrRes);
        transposeRealTemplate<uint64>(dimsA, (uint64*)A.dataPointer(), (uint64*)Res.dataPointer());
    } break;
    case NLS_INT64: {
        int64* ptrRes = (int64*)ArrayOf::allocateArrayOf(classA, dimsRes.elementCount());
        Res = ArrayOf(classA, dimsRes, ptrRes);
        transposeRealTemplate<int64>(dimsA, (int64*)A.dataPointer(), (int64*)Res.dataPointer());
    } break;
    case NLS_SINGLE: {
        if (A.isSparse()) {
            return TransposeSparseDouble(A);
        }
        single* ptrRes = (single*)ArrayOf::allocateArrayOf(classA, dimsRes.elementCount());
        Res = ArrayOf(classA, dimsRes, ptrRes);
        transposeRealTemplate<single>(dimsA, (single*)A.dataPointer(), (single*)Res.dataPointer());
    } break;
    case NLS_DOUBLE: {
        if (A.isSparse()) {
            return TransposeSparseDouble(A);
        }
        double* ptrRes = (double*)ArrayOf::allocateArrayOf(classA, dimsRes.elementCount());
        Res = ArrayOf(classA, dimsRes, ptrRes);
        transposeRealTemplate<double>(dimsA, (double*)A.dataPointer(), (double*)Res.dataPointer());
    } break;
    case NLS_SCOMPLEX: {
        if (A.isSparse()) {
            return TransposeSparseDouble(A);
        }
        single* ptrRes = (single*)ArrayOf::allocateArrayOf(classA, dimsRes.elementCount() * 2);
        Res = ArrayOf(classA, dimsRes, ptrRes);
        transposeComplexTemplate<single>(
            dimsA, (single*)A.dataPointer(), (single*)Res.dataPointer());
    } break;
    case NLS_DCOMPLEX: {
        if (A.isSparse()) {
            return TransposeSparseDouble(A);
        }
        double* ptrRes = (double*)ArrayOf::allocateArrayOf(classA, dimsRes.elementCount() * 2);
        Res = ArrayOf(classA, dimsRes, ptrRes);
        transposeComplexTemplate<double>(
            dimsA, (double*)A.dataPointer(), (double*)Res.dataPointer());
    } break;
    case NLS_CHAR: {
        charType* ptrRes = (charType*)ArrayOf::allocateArrayOf(classA, dimsRes.elementCount());
        Res = ArrayOf(classA, dimsRes, ptrRes);
        transposeRealTemplate<charType>(
            dimsA, (charType*)A.dataPointer(), (charType*)Res.dataPointer());
    } break;
    case NLS_CLASS_ARRAY:
    case NLS_CLASSDEF_ARRAY: {
        stringVector fieldnames = A.fieldNames();
        void* destPtr = ArrayOf::allocateArrayOf(classA, dimsRes.elementCount(), fieldnames, false);
        Res = ArrayOf(classA, dimsRes, destPtr, false, fieldnames);
        Res.setClassType(A.classType());
        if (classA == NLS_CLASSDEF_ARRAY) {
            Res.setClassdefLayout(A.classdefLayout());
        }
        indexType rowCount = dimsA.rows();
        indexType colCount = dimsA.columns();
        ArrayOf RR = A;
        int ptr = 0;
        for (indexType i = 0; i < rowCount; i++) {
            for (indexType j = 0; j < colCount; j++) {
                RR.copyElements(i + j * rowCount, destPtr, ptr, 1);
                ptr++;
            }
        }
        return Res;
    } break;
    case NLS_FUNCTION_HANDLE:
    case NLS_HANDLE:
    case NLS_STRUCT_ARRAY:
    case NLS_STRING_ARRAY:
    case NLS_CELL_ARRAY: {
        Res = A;
        Res.ensureSingleOwner();
        void* destPtr = (void*)Res.dataPointer();
        indexType rowCount = dimsA.rows();
        indexType colCount = dimsA.columns();
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
