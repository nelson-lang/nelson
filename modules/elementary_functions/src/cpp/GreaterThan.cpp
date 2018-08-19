//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "GreaterThan.hpp"
#include "complex_abs.hpp"
#include "MatrixCheck.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "Exception.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
void
greaterThanReal(indexType N, logical* C, const T* A, int stride1, const T* B, int stride2)
{
    indexType m = 0, p = 0;
#if defined(__NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
    for (indexType i = 0; i < N; i++) {
        C[i] = (A[m] > B[p]) ? logical(1) : logical(0);
        m += stride1;
        p += stride2;
    }
}
//=============================================================================
template <class T>
void
greaterThanComplex(indexType N, logical* C, const T* A, int stride1, const T* B, int stride2)
{
    indexType m = 0, p = 0;
#if defined(__NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
    for (indexType i = 0; i < N; i++) {
        C[i] = (complex_abs<T>(A[2 * m], A[2 * m + 1]) > complex_abs<T>(B[2 * p], B[2 * p + 1]))
            ? logical(1)
            : logical(0);
        m += stride1;
        p += stride2;
    }
}
//=============================================================================
ArrayOf
GreaterThan(ArrayOf& A, ArrayOf& B, bool& needToOverload)
{
    needToOverload = false;
    VectorCheck(A, B, ">");
    Class classCommon = FindCommonType(A, B, false);
    A.promoteType(classCommon);
    B.promoteType(classCommon);
    int Astride, Bstride;
    indexType Clen = 0;
    Dimensions Cdim;
    if (A.isScalar()) {
        Astride = 0;
        Bstride = 1;
        Cdim = B.getDimensions();
    } else if (B.isScalar()) {
        Astride = 1;
        Bstride = 0;
        Cdim = A.getDimensions();
    } else {
        Astride = 1;
        Bstride = 1;
        Cdim = A.getDimensions();
    }
    Clen = Cdim.getElementCount();
    void* Cp = new_with_exception<logical>(Clen);
    switch (B.getDataClass()) {
    case NLS_LOGICAL: {
        greaterThanReal<logical>(Clen, (logical*)Cp, (logical*)A.getDataPointer(), Astride,
            (logical*)B.getDataPointer(), Bstride);
    } break;
    case NLS_CHAR: {
        greaterThanReal<charType>(Clen, (logical*)Cp, (charType*)A.getDataPointer(), Astride,
            (charType*)B.getDataPointer(), Bstride);
    } break;
    case NLS_UINT8: {
        greaterThanReal<uint8>(Clen, (logical*)Cp, (uint8*)A.getDataPointer(), Astride,
            (uint8*)B.getDataPointer(), Bstride);
    } break;
    case NLS_UINT16: {
        greaterThanReal<uint16>(Clen, (logical*)Cp, (uint16*)A.getDataPointer(), Astride,
            (uint16*)B.getDataPointer(), Bstride);
    } break;
    case NLS_UINT32: {
        greaterThanReal<uint32>(Clen, (logical*)Cp, (uint32*)A.getDataPointer(), Astride,
            (uint32*)B.getDataPointer(), Bstride);
    } break;
    case NLS_UINT64: {
        greaterThanReal<uint64>(Clen, (logical*)Cp, (uint64*)A.getDataPointer(), Astride,
            (uint64*)B.getDataPointer(), Bstride);
    } break;
	case NLS_INT8: {
        greaterThanReal<int8>(Clen, (logical*)Cp, (int8*)A.getDataPointer(), Astride,
            (int8*)B.getDataPointer(), Bstride);
    } break;
    case NLS_INT16: {
        greaterThanReal<int16>(Clen, (logical*)Cp, (int16*)A.getDataPointer(), Astride,
            (int16*)B.getDataPointer(), Bstride);
    } break;
    case NLS_INT32: {
        greaterThanReal<int32>(Clen, (logical*)Cp, (int32*)A.getDataPointer(), Astride,
            (int32*)B.getDataPointer(), Bstride);
    } break;
	case NLS_INT64: {
        greaterThanReal<int64>(Clen, (logical*)Cp, (int64*)A.getDataPointer(), Astride,
            (int64*)B.getDataPointer(), Bstride);
    } break;
    case NLS_SINGLE: {
        greaterThanReal<single>(Clen, (logical*)Cp, (single*)A.getDataPointer(), Astride,
            (single*)B.getDataPointer(), Bstride);
    } break;
    case NLS_DOUBLE: {
        greaterThanReal<double>(Clen, (logical*)Cp, (double*)A.getDataPointer(), Astride,
            (double*)B.getDataPointer(), Bstride);
    } break;
    case NLS_SCOMPLEX: {
        greaterThanComplex<single>(Clen, (logical*)Cp, (single*)A.getDataPointer(), Astride,
            (single*)B.getDataPointer(), Bstride);
    } break;
    case NLS_DCOMPLEX: {
        greaterThanComplex<double>(Clen, (logical*)Cp, (double*)A.getDataPointer(), Astride,
            (double*)B.getDataPointer(), Bstride);
    } break;
    default: {
        needToOverload = true;
    } break;
    }
    return ArrayOf(NLS_LOGICAL, Cdim, Cp);
}
//=============================================================================
}
//=============================================================================
