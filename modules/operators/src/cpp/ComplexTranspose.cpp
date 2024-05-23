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
#include "i18n.hpp"
#include "Error.hpp"
#include "CtransposeSparseDouble.hpp"
#include "CtransposeSparseLogical.hpp"
#include "ComplexTranspose.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
void
complexTransposeRealTemplate(const Dimensions& dimsA, T* ptrA, T* ptrRes)
{
    ompIndexType nbRows = dimsA.getRows();
    ompIndexType nbColumns = dimsA.getColumns();
    ompIndexType i = 0;
    ompIndexType j = 0;
#if WITH_OPENMP
#pragma omp parallel for private(j)
#endif
    for (i = 0; i < nbRows; i++) {
        for (j = 0; j < nbColumns; j++) {
            ptrRes[i * nbColumns + j] = ptrA[j * nbRows + i];
        }
    }
}
//=============================================================================
template <class T>
void
complexTransposeComplexTemplate(const Dimensions& dimsA, T* ptrA, T* ptrRes)
{
    auto* matCplxA = reinterpret_cast<std::complex<T>*>(ptrA);
    auto* matCplxRes = reinterpret_cast<std::complex<T>*>(ptrRes);
    ompIndexType nbRows = dimsA.getRows();
    ompIndexType nbColumns = dimsA.getColumns();
    ompIndexType i = 0;
    ompIndexType j = 0;
#if WITH_OPENMP
#pragma omp parallel for private(j)
#endif
    for (i = 0; i < nbRows; i++) {
        for (j = 0; j < nbColumns; j++) {
            matCplxRes[i * nbColumns + j].real(matCplxA[j * nbRows + i].real());
            matCplxRes[i * nbColumns + j].imag(-matCplxA[j * nbRows + i].imag());
        }
    }
}
//=============================================================================
ArrayOf
ComplexTranspose(const ArrayOf& A, bool& needToOverload)
{
    /* Code not factorized with Transpose to speed up at runtime */
    needToOverload = false;
    NelsonType classA = A.getDataClass();
    if ((classA > NLS_CHAR) && !(A.isCell() || A.isStruct() || A.isStringArray())) {
        needToOverload = true;
        return {};
    }
    Dimensions dimsA = A.getDimensions();
    bool isSupported = (A.isEmpty() || A.isScalar() || A.is2D());
    if (!isSupported) {
        std::wstring msg = _W("ctranspose on N-D array is undefined.");
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
            return CtransposeSparseLogical(A);
        }
        logical* ptrRes = static_cast<logical*>(const_cast<void*>(
            static_cast<const void*>(ArrayOf::allocateArrayOf(classA, dimsRes.getElementCount()))));
        Res = ArrayOf(classA, dimsRes, ptrRes);
        complexTransposeRealTemplate<logical>(
            dimsA, (logical*)A.getDataPointer(), (logical*)Res.getDataPointer());
    } break;
    case NLS_UINT8: {
        uint8* ptrRes = static_cast<uint8*>(const_cast<void*>(
            static_cast<const void*>(ArrayOf::allocateArrayOf(classA, dimsRes.getElementCount()))));
        Res = ArrayOf(classA, dimsRes, ptrRes);
        complexTransposeRealTemplate<uint8>(
            dimsA, (uint8*)A.getDataPointer(), (uint8*)Res.getDataPointer());
    } break;
    case NLS_INT8: {
        int8* ptrRes = static_cast<int8*>(const_cast<void*>(
            static_cast<const void*>(ArrayOf::allocateArrayOf(classA, dimsRes.getElementCount()))));
        Res = ArrayOf(classA, dimsRes, ptrRes);
        complexTransposeRealTemplate<int8>(
            dimsA, (int8*)A.getDataPointer(), (int8*)Res.getDataPointer());
    } break;
    case NLS_UINT16: {
        uint16* ptrRes = static_cast<uint16*>(const_cast<void*>(
            static_cast<const void*>(ArrayOf::allocateArrayOf(classA, dimsRes.getElementCount()))));
        Res = ArrayOf(classA, dimsRes, ptrRes);
        complexTransposeRealTemplate<uint16>(
            dimsA, (uint16*)A.getDataPointer(), (uint16*)Res.getDataPointer());
    } break;
    case NLS_INT16: {
        int16* ptrRes = static_cast<int16*>(const_cast<void*>(
            static_cast<const void*>(ArrayOf::allocateArrayOf(classA, dimsRes.getElementCount()))));
        Res = ArrayOf(classA, dimsRes, ptrRes);
        complexTransposeRealTemplate<int16>(
            dimsA, (int16*)A.getDataPointer(), (int16*)Res.getDataPointer());
    } break;
    case NLS_UINT32: {
        uint32* ptrRes = static_cast<uint32*>(const_cast<void*>(
            static_cast<const void*>(ArrayOf::allocateArrayOf(classA, dimsRes.getElementCount()))));
        Res = ArrayOf(classA, dimsRes, ptrRes);
        complexTransposeRealTemplate<uint32>(
            dimsA, (uint32*)A.getDataPointer(), (uint32*)Res.getDataPointer());
    } break;
    case NLS_INT32: {
        int32* ptrRes = static_cast<int32*>(const_cast<void*>(
            static_cast<const void*>(ArrayOf::allocateArrayOf(classA, dimsRes.getElementCount()))));
        Res = ArrayOf(classA, dimsRes, ptrRes);
        complexTransposeRealTemplate<int32>(
            dimsA, (int32*)A.getDataPointer(), (int32*)Res.getDataPointer());
    } break;
    case NLS_UINT64: {
        uint64* ptrRes = static_cast<uint64*>(const_cast<void*>(
            static_cast<const void*>(ArrayOf::allocateArrayOf(classA, dimsRes.getElementCount()))));
        Res = ArrayOf(classA, dimsRes, ptrRes);
        complexTransposeRealTemplate<uint64>(
            dimsA, (uint64*)A.getDataPointer(), (uint64*)Res.getDataPointer());
    } break;
    case NLS_INT64: {
        int64* ptrRes = static_cast<int64*>(const_cast<void*>(
            static_cast<const void*>(ArrayOf::allocateArrayOf(classA, dimsRes.getElementCount()))));
        Res = ArrayOf(classA, dimsRes, ptrRes);
        complexTransposeRealTemplate<int64>(
            dimsA, (int64*)A.getDataPointer(), (int64*)Res.getDataPointer());
    } break;
    case NLS_SINGLE: {
        single* ptrRes = static_cast<single*>(const_cast<void*>(
            static_cast<const void*>(ArrayOf::allocateArrayOf(classA, dimsRes.getElementCount()))));
        Res = ArrayOf(classA, dimsRes, ptrRes);
        complexTransposeRealTemplate<single>(
            dimsA, (single*)A.getDataPointer(), (single*)Res.getDataPointer());
    } break;
    case NLS_DOUBLE: {
        if (A.isSparse()) {
            return CtransposeSparseDouble(A);
        }
        double* ptrRes = static_cast<double*>(const_cast<void*>(
            static_cast<const void*>(ArrayOf::allocateArrayOf(classA, dimsRes.getElementCount()))));
        Res = ArrayOf(classA, dimsRes, ptrRes);
        complexTransposeRealTemplate<double>(
            dimsA, (double*)A.getDataPointer(), (double*)Res.getDataPointer());
    } break;
    case NLS_SCOMPLEX: {
        single* ptrRes = static_cast<single*>(const_cast<void*>(static_cast<const void*>(
            ArrayOf::allocateArrayOf(classA, dimsRes.getElementCount() * 2))));
        Res = ArrayOf(classA, dimsRes, ptrRes);
        complexTransposeComplexTemplate<single>(
            dimsA, (single*)A.getDataPointer(), (single*)Res.getDataPointer());
    } break;
    case NLS_DCOMPLEX: {
        if (A.isSparse()) {
            return CtransposeSparseDouble(A);
        }
        double* ptrRes = static_cast<double*>(const_cast<void*>(static_cast<const void*>(
            ArrayOf::allocateArrayOf(classA, dimsRes.getElementCount() * 2))));
        Res = ArrayOf(classA, dimsRes, ptrRes);
        complexTransposeComplexTemplate<double>(
            dimsA, (double*)A.getDataPointer(), (double*)Res.getDataPointer());
    } break;
    case NLS_CHAR: {
        charType* ptrRes = static_cast<charType*>(const_cast<void*>(
            static_cast<const void*>(ArrayOf::allocateArrayOf(classA, dimsRes.getElementCount()))));
        Res = ArrayOf(classA, dimsRes, ptrRes);
        complexTransposeRealTemplate<charType>(
            dimsA, (charType*)A.getDataPointer(), (charType*)Res.getDataPointer());
    } break;
    case NLS_FUNCTION_HANDLE:
    case NLS_CLASS_ARRAY:
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
