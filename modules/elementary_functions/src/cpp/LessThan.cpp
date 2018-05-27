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
#include "LessThan.hpp"
#include "MatrixCheck.hpp"
#include "complex_abs.hpp"
#include "ClassName.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
void
lessThanReal(indexType N, logical* C, const T* A, int stride1, const T* B, int stride2)
{
    indexType m = 0, p = 0;
#if defined(__NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
    for (indexType i = 0; i < N; i++) {
        C[i] = (A[m] < B[p]) ? logical(1) : logical(0);
        m += stride1;
        p += stride2;
    }
}
//=============================================================================
template <class T>
void
lessThanComplex(indexType N, logical* C, const T* A, int stride1, const T* B, int stride2)
{
    indexType m = 0, p = 0;
#if defined(__NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
    for (indexType i = 0; i < N; i++) {
        C[i] = (complex_abs<T>(A[2 * m], A[2 * m + 1]) < complex_abs<T>(B[2 * p], B[2 * p + 1]))
            ? logical(1)
            : logical(0);
        m += stride1;
        p += stride2;
    }
}
//=============================================================================
ArrayOf
LessThan(ArrayOf& A, ArrayOf& B, bool mustRaiseError, bool& bSuccess)
{
    VectorCheck(A, B, "<");
    Class classCommon = FindCommonType(A, B, false);
    if (A.isSparse() || B.isSparse()) {
        std::string overload = ClassName(A) + "_lt_" + ClassName(B);
        throw Exception(_("function") + " " + overload + " " + _("undefined."));
    }
    try {
        A.promoteType(classCommon);
        B.promoteType(classCommon);
    } catch (Exception) {
        if (mustRaiseError) {
            throw;
        } else {
            bSuccess = false;
            return ArrayOf();
        }
    }
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
    case NLS_INT64: {
        lessThanReal<int64>(Clen, (logical*)Cp, (int64*)A.getDataPointer(), Astride,
            (int64*)B.getDataPointer(), Bstride);
    } break;
    case NLS_SINGLE: {
        lessThanReal<single>(Clen, (logical*)Cp, (single*)A.getDataPointer(), Astride,
            (single*)B.getDataPointer(), Bstride);
    } break;
    case NLS_DOUBLE: {
        lessThanReal<double>(Clen, (logical*)Cp, (double*)A.getDataPointer(), Astride,
            (double*)B.getDataPointer(), Bstride);
    } break;
    case NLS_SCOMPLEX: {
        lessThanComplex<single>(Clen, (logical*)Cp, (single*)A.getDataPointer(), Astride,
            (single*)B.getDataPointer(), Bstride);
    } break;
    case NLS_DCOMPLEX: {
        lessThanComplex<double>(Clen, (logical*)Cp, (double*)A.getDataPointer(), Astride,
            (double*)B.getDataPointer(), Bstride);
    } break;
    default: {
        if (mustRaiseError) {
            std::string overload = ClassName(A) + "_lt_" + ClassName(B);
            throw Exception(_("function") + " " + overload + " " + _("undefined."));
        } else {
            bSuccess = false;
            return ArrayOf();
        }
    } break;
    }
    bSuccess = true;
    return ArrayOf(NLS_LOGICAL, Cdim, Cp);
}
}
//=============================================================================
