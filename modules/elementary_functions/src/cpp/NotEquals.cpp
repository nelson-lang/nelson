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
#include "NotEquals.hpp"
#include "MatrixCheck.hpp"
#include "Exception.hpp"
#include "ClassName.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
void
notEqualsReal(indexType N, logical* C, const T* A, int stride1, const T* B, int stride2)
{
    if ((stride1 == 1) && (stride2 == 1)) {
#if defined(__NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (indexType i = 0; i < N; i++) {
            C[i] = (A[i] != B[i]) ? logical(1) : logical(0);
        }
    } else {
        if (stride1) {
#if defined(__NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
            for (indexType i = 0; i < N; i++) {
                C[i] = (A[i] != B[0]) ? logical(1) : logical(0);
            }
        } else {
#if defined(__NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
            for (indexType i = 0; i < N; i++) {
                C[i] = (A[0] != B[i]) ? logical(1) : logical(0);
            }
        }
    }
}
//=============================================================================
template <class T>
void
notEqualsComplex(indexType N, logical* C, const T* A, int stride1, const T* B, int stride2)
{
    if ((stride1 == 1) && (stride2 == 1)) {
#if defined(__NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (indexType i = 0; i < N; i++) {
            C[i] = ((A[2 * i] != B[2 * i]) || (A[2 * i + 1] != B[2 * i + 1])) ? logical(1)
                                                                              : logical(0);
        }
    } else {
        if (stride1 == 1) {
#if defined(__NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
            for (indexType i = 0; i < N; i++) {
                C[i] = ((A[2 * i] != B[0]) || (A[2 * i + 1] != B[1])) ? logical(1) : logical(0);
            }
        } else {
#if defined(__NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
            for (indexType i = 0; i < N; i++) {
                C[i] = ((A[0] != B[2 * i]) || (A[1] != B[2 * i + 1])) ? logical(1) : logical(0);
            }
        }
    }
}
//=============================================================================
ArrayOf
NotEquals(ArrayOf& A, ArrayOf& B, bool& needToOverload)
{
    needToOverload = false;
    if (A.isReferenceType() || B.isReferenceType()) {
        needToOverload = true;
        return ArrayOf();
    }
    VectorCheck(A, B, "~=");
    Class classCommon = FindCommonType(A, B, false);
    A.promoteType(classCommon);
    B.promoteType(classCommon);
    int Astride = 0, Bstride = 0;
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
        notEqualsReal<logical>(Clen, (logical*)Cp, (logical*)A.getDataPointer(), Astride,
            (logical*)B.getDataPointer(), Bstride);
    } break;
    case NLS_UINT8: {
        notEqualsReal<uint8>(Clen, (logical*)Cp, (uint8*)A.getDataPointer(), Astride,
            (uint8*)B.getDataPointer(), Bstride);
    } break;
    case NLS_INT8: {
        notEqualsReal<int8>(Clen, (logical*)Cp, (int8*)A.getDataPointer(), Astride,
            (int8*)B.getDataPointer(), Bstride);
    } break;
    case NLS_UINT16: {
        notEqualsReal<uint16>(Clen, (logical*)Cp, (uint16*)A.getDataPointer(), Astride,
            (uint16*)B.getDataPointer(), Bstride);
    } break;
    case NLS_INT16: {
        notEqualsReal<int16>(Clen, (logical*)Cp, (int16*)A.getDataPointer(), Astride,
            (int16*)B.getDataPointer(), Bstride);
    } break;
    case NLS_UINT32: {
        notEqualsReal<uint32>(Clen, (logical*)Cp, (uint32*)A.getDataPointer(), Astride,
            (uint32*)B.getDataPointer(), Bstride);
    } break;
    case NLS_INT32: {
        notEqualsReal<int32>(Clen, (logical*)Cp, (int32*)A.getDataPointer(), Astride,
            (int32*)B.getDataPointer(), Bstride);
    } break;
    case NLS_UINT64: {
        notEqualsReal<uint64>(Clen, (logical*)Cp, (uint64*)A.getDataPointer(), Astride,
            (uint64*)B.getDataPointer(), Bstride);
    } break;
    case NLS_INT64: {
        notEqualsReal<int64>(Clen, (logical*)Cp, (int64*)A.getDataPointer(), Astride,
            (int64*)B.getDataPointer(), Bstride);
    } break;
    case NLS_SINGLE: {
        notEqualsReal<float>(Clen, (logical*)Cp, (float*)A.getDataPointer(), Astride,
            (float*)B.getDataPointer(), Bstride);
    } break;
    case NLS_DOUBLE: {
        notEqualsReal<double>(Clen, (logical*)Cp, (double*)A.getDataPointer(), Astride,
            (double*)B.getDataPointer(), Bstride);
    } break;
    case NLS_SCOMPLEX: {
        notEqualsComplex<float>(Clen, (logical*)Cp, (float*)A.getDataPointer(), Astride,
            (float*)B.getDataPointer(), Bstride);
    } break;
    case NLS_DCOMPLEX: {
        notEqualsComplex<double>(Clen, (logical*)Cp, (double*)A.getDataPointer(), Astride,
            (double*)B.getDataPointer(), Bstride);
    } break;
    case NLS_CHAR: {
        notEqualsReal<charType>(Clen, (logical*)Cp, (charType*)A.getDataPointer(), Astride,
            (charType*)B.getDataPointer(), Bstride);
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
