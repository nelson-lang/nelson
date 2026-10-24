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
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "ComplexConstructor.hpp"
#include "ClassName.hpp"
#include "characters_encoding.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
void
ComplexArrayArray(T* A, T* B, T* C, size_t len)
{
    size_t l = 0;
    for (size_t k = 0; k < len; k++) {
        C[l] = A[k];
        C[l + 1] = B[k];
        l = l + 2;
    }
}
//=============================================================================
template <class T>
void
ComplexScalarArray(T A, T* B, T* C, size_t len)
{
    size_t l = 0;
    for (size_t k = 0; k < len; k++) {
        C[l] = A;
        C[l + 1] = B[k];
        l = l + 2;
    }
}
//=============================================================================
template <class T>
void
ComplexArrayScalar(T* A, T B, T* C, size_t len)
{
    size_t l = 0;
    for (size_t k = 0; k < len; k++) {
        C[l] = A[k];
        C[l + 1] = B;
        l = l + 2;
    }
}
//=============================================================================
ArrayOf
ComplexConstructor(ArrayOf arrayA, ArrayOf arrayB)
{
    ArrayOf res;
    if (arrayA.isSparse() || arrayB.isSparse()) {
        Error(L"Nelson:elementary_functions:inputArgumentsFull", L"Input arguments must be full.");
    }
    if (!arrayA.isNumeric() || !arrayB.isNumeric()) {
        Error(L"Nelson:validators:mustBeNumeric", L"%s must be numeric.", L"Input arguments");
    }
    if ((arrayA.dataClass() == NLS_SCOMPLEX) || (arrayA.dataClass() == NLS_DCOMPLEX)
        || (arrayB.dataClass() == NLS_SCOMPLEX) || (arrayB.dataClass() == NLS_DCOMPLEX)) {
        Error(L"Nelson:validators:mustBeReal", L"%s must be real.", L"Input arguments");
    }
    NelsonType DestinationClass = NLS_DCOMPLEX;
    if ((arrayA.dataClass() == NLS_SINGLE) || (arrayB.dataClass() == NLS_SINGLE)) {
        DestinationClass = NLS_SCOMPLEX;
    }
    if (!(arrayA.dimensions().equals(arrayB.dimensions()) || arrayA.isScalar()
            || arrayB.isScalar())) {
        Error(L"Nelson:elementary_functions:inputArgumentsSameSize",
            L"Input arguments must have the same size.");
    }
    if (DestinationClass == NLS_SCOMPLEX) {
        arrayA.promoteType(NLS_SINGLE);
        arrayB.promoteType(NLS_SINGLE);
    } else {
        arrayA.promoteType(NLS_DOUBLE);
        arrayB.promoteType(NLS_DOUBLE);
    }
    void* ptrdst = nullptr;
    Dimensions DestinationDims;
    if (arrayA.dimensions().equals(arrayB.dimensions())) {
        DestinationDims = arrayA.dimensions();
        size_t len = arrayA.elementCount();
        ptrdst = ArrayOf::allocateArrayOf(DestinationClass, len, stringVector(), true);
        if (DestinationClass == NLS_SCOMPLEX) {
            auto* psA = (single*)arrayA.dataPointer();
            auto* psB = (single*)arrayB.dataPointer();
            auto* ps = static_cast<single*>(ptrdst);
            ComplexArrayArray<single>(psA, psB, ps, len);
        } else {
            auto* pdA = (double*)arrayA.dataPointer();
            auto* pdB = (double*)arrayB.dataPointer();
            auto* pd = static_cast<double*>(ptrdst);
            ComplexArrayArray<double>(pdA, pdB, pd, len);
        }
    } else if (arrayA.isScalar()) {
        DestinationDims = arrayB.dimensions();
        size_t len = arrayB.elementCount();
        ptrdst = ArrayOf::allocateArrayOf(DestinationClass, len, stringVector(), true);
        if (DestinationClass == NLS_SCOMPLEX) {
            auto* ps = static_cast<single*>(ptrdst);
            single A = arrayA.asSingleScalar();
            auto* psB = (single*)arrayB.dataPointer();
            ComplexScalarArray<single>(A, psB, ps, len);
        } else {
            auto* pd = static_cast<double*>(ptrdst);
            double A = arrayA.asDoubleScalar();
            auto* pdB = (double*)arrayB.dataPointer();
            ComplexScalarArray<double>(A, pdB, pd, len);
        }
    } else /* (arrayB.isScalar()) */
    {
        DestinationDims = arrayA.dimensions();
        size_t len = arrayA.elementCount();
        ptrdst = ArrayOf::allocateArrayOf(DestinationClass, len, stringVector(), true);
        if (DestinationClass == NLS_SCOMPLEX) {
            auto* ps = static_cast<single*>(ptrdst);
            auto* psA = (single*)arrayA.dataPointer();
            single B = arrayB.asSingleScalar();
            ComplexArrayScalar<single>(psA, B, ps, len);
        } else {
            auto* pd = static_cast<double*>(ptrdst);
            auto* pdA = (double*)arrayA.dataPointer();
            double B = arrayB.asDoubleScalar();
            ComplexArrayScalar<double>(pdA, B, pd, len);
        }
    }
    res = ArrayOf(DestinationClass, DestinationDims, ptrdst);
    return res;
}
//=============================================================================
ArrayOf
ComplexConstructor(const ArrayOf& arrayA)
{
    ArrayOf res;
    switch (arrayA.dataClass()) {
    case NLS_SCOMPLEX:
    case NLS_DCOMPLEX: {
        res = arrayA;
    } break;
    case NLS_HANDLE:
    case NLS_GO_HANDLE:
    case NLS_CELL_ARRAY:
    case NLS_STRING_ARRAY:
    case NLS_STRUCT_ARRAY:
    case NLS_CLASS_ARRAY:
    case NLS_CLASSDEF_ARRAY:
    case NLS_FUNCTION_HANDLE:
    case NLS_CHAR:
    case NLS_LOGICAL:
    default: {
        Error(
            "Nelson:UndefinedFunction", "Undefined function '%s'", ClassName(arrayA) + "_complex");
    } break;
    case NLS_DOUBLE:
    case NLS_UINT8:
    case NLS_INT8:
    case NLS_UINT16:
    case NLS_INT16:
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_UINT64:
    case NLS_INT64: {
        res = arrayA;
        res.promoteType(NLS_DCOMPLEX);
    } break;
    case NLS_SINGLE: {
        res = arrayA;
        res.promoteType(NLS_SCOMPLEX);
    } break;
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
