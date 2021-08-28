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
#include <fmt/printf.h>
#include <fmt/format.h>
#include "ComplexConstructor.hpp"
#include "ClassName.hpp"
#include "characters_encoding.hpp"
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
        Error(_W("Input arguments must be full."));
    }
    if (!arrayA.isNumeric() || !arrayB.isNumeric()) {
        Error(_W("Input arguments must be numeric."));
    }
    if ((arrayA.getDataClass() == NLS_SCOMPLEX) || (arrayA.getDataClass() == NLS_DCOMPLEX)
        || (arrayB.getDataClass() == NLS_SCOMPLEX) || (arrayB.getDataClass() == NLS_DCOMPLEX)) {
        Error(_W("Input arguments must be real."));
    }
    Class DestinationClass = NLS_DCOMPLEX;
    if ((arrayA.getDataClass() == NLS_SINGLE) || (arrayB.getDataClass() == NLS_SINGLE)) {
        DestinationClass = NLS_SCOMPLEX;
    }
    if (!(arrayA.getDimensions().equals(arrayB.getDimensions()) || arrayA.isScalar()
            || arrayB.isScalar())) {
        Error(_W("Input arguments must have the same size."));
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
    if (arrayA.getDimensions().equals(arrayB.getDimensions())) {
        DestinationDims = arrayA.getDimensions();
        size_t len = arrayA.getElementCount();
        ptrdst = ArrayOf::allocateArrayOf(DestinationClass, len, stringVector(), true);
        if (DestinationClass == NLS_SCOMPLEX) {
            auto* psA = (single*)arrayA.getDataPointer();
            auto* psB = (single*)arrayB.getDataPointer();
            auto* ps = static_cast<single*>(ptrdst);
            ComplexArrayArray<single>(psA, psB, ps, len);
        } else {
            auto* pdA = (double*)arrayA.getDataPointer();
            auto* pdB = (double*)arrayB.getDataPointer();
            auto* pd = static_cast<double*>(ptrdst);
            ComplexArrayArray<double>(pdA, pdB, pd, len);
        }
    } else if (arrayA.isScalar()) {
        DestinationDims = arrayB.getDimensions();
        size_t len = arrayB.getElementCount();
        ptrdst = ArrayOf::allocateArrayOf(DestinationClass, len, stringVector(), true);
        if (DestinationClass == NLS_SCOMPLEX) {
            auto* ps = static_cast<single*>(ptrdst);
            single A = arrayA.getContentAsSingleScalar();
            auto* psB = (single*)arrayB.getDataPointer();
            ComplexScalarArray<single>(A, psB, ps, len);
        } else {
            auto* pd = static_cast<double*>(ptrdst);
            double A = arrayA.getContentAsDoubleScalar();
            auto* pdB = (double*)arrayB.getDataPointer();
            ComplexScalarArray<double>(A, pdB, pd, len);
        }
    } else /* (arrayB.isScalar()) */
    {
        DestinationDims = arrayA.getDimensions();
        size_t len = arrayA.getElementCount();
        ptrdst = ArrayOf::allocateArrayOf(DestinationClass, len, stringVector(), true);
        if (DestinationClass == NLS_SCOMPLEX) {
            auto* ps = static_cast<single*>(ptrdst);
            auto* psA = (single*)arrayA.getDataPointer();
            single B = arrayB.getContentAsSingleScalar();
            ComplexArrayScalar<single>(psA, B, ps, len);
        } else {
            auto* pd = static_cast<double*>(ptrdst);
            auto* pdA = (double*)arrayA.getDataPointer();
            double B = arrayB.getContentAsDoubleScalar();
            ComplexArrayScalar<double>(pdA, B, pd, len);
        }
    }
    res = ArrayOf(DestinationClass, DestinationDims, ptrdst);
    return res;
}
//=============================================================================
ArrayOf
ComplexConstructor(ArrayOf arrayA)
{
    ArrayOf res;
    switch (arrayA.getDataClass()) {
    case NLS_SCOMPLEX:
    case NLS_DCOMPLEX: {
        res = arrayA;
    } break;
    case NLS_HANDLE:
    case NLS_GO_HANDLE:
    case NLS_CELL_ARRAY:
    case NLS_STRING_ARRAY:
    case NLS_STRUCT_ARRAY:
    case NLS_CHAR:
    case NLS_LOGICAL:
    default: {
        Error(fmt::sprintf(_("Undefined function '%s_complex'"), ClassName(arrayA)));
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
