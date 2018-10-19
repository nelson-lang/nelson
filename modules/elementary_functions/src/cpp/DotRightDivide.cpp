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
#include "DotRightDivide.hpp"
#include "MatrixCheck.hpp"
#include "Exception.hpp"

//=============================================================================
namespace Nelson {
template <class T>
void
dividefullreal(indexType N, T* C, const T* A, int stride1, const T* B, int stride2)
{
    indexType m, p;
    m = 0;
    p = 0;
    for (indexType i = 0; i < N; i++) {
        C[i] = A[m] / B[p];
        m += stride1;
        p += stride2;
    }
}

template <class T>
void
complex_divide(T* c, const T* a, const T* b)
{
    double ratio, den;
    double abr, abi, cr;
    if ((abr = b[0]) < 0.) {
        abr = -abr;
    }
    if ((abi = b[1]) < 0.) {
        abi = -abi;
    }
    if (abr <= abi) {
        if (abi == 0) {
            if (a[1] != 0 || a[0] != 0) {
                abi = 1.;
            }
            c[1] = c[0] = (float)(abi / abr);
            return;
        }
        ratio = b[0] / b[1];
        den = b[1] * (1 + ratio * ratio);
        cr = (a[0] * ratio + a[1]) / den;
        c[1] = (T)((a[1] * ratio - a[0]) / den);
    } else {
        ratio = b[1] / b[0];
        den = b[0] * (1 + ratio * ratio);
        cr = (a[0] + a[1] * ratio) / den;
        c[1] = (T)((a[1] - a[0] * ratio) / den);
    }
    c[0] = (T)(cr);
}

template <class T>
void
dividefullcomplex(indexType N, T* C, const T* A, int stride1, const T* B, int stride2)
{
    indexType m, p;
    m = 0;
    p = 0;
    for (indexType i = 0; i < N; i++) {
        complex_divide<T>(C + 2 * i, A + 2 * m, B + 2 * p);
        m += stride1;
        p += stride2;
    }
}

ArrayOf
DotRightDivide(ArrayOf A, ArrayOf B)
{
    // Process the two arguments through the type check and dimension checks...
    VectorCheck(A, B, "./");
    Class commonClass = FindCommonType(A, B, true);
    A.promoteType(commonClass);
    B.promoteType(commonClass);
    // Get a pointer to the function we ultimately need to execute
    int Astride, Bstride;
    void* Cp = nullptr;
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
    // Cp = Malloc(Clen*B.getElementSize());
    switch (B.getDataClass()) {
    case NLS_INT32:
        Cp = new_with_exception<int32>(Clen);
        dividefullreal<int32>(Clen, (int32*)Cp, (int32*)A.getDataPointer(), Astride,
            (int32*)B.getDataPointer(), Bstride);
        break;
    case NLS_SINGLE:
        Cp = new_with_exception<float>(Clen);
        dividefullreal<float>(Clen, (float*)Cp, (float*)A.getDataPointer(), Astride,
            (float*)B.getDataPointer(), Bstride);
        break;
    case NLS_DOUBLE:
        Cp = new_with_exception<double>(Clen);
        dividefullreal<double>(Clen, (double*)Cp, (double*)A.getDataPointer(), Astride,
            (double*)B.getDataPointer(), Bstride);
        break;
    case NLS_SCOMPLEX:
        Cp = new_with_exception<float>(Clen * 2);
        dividefullcomplex<float>(Clen, (float*)Cp, (float*)A.getDataPointer(), Astride,
            (float*)B.getDataPointer(), Bstride);
        break;
    case NLS_DCOMPLEX:
        Cp = new_with_exception<double>(Clen * 2);
        dividefullcomplex<double>(Clen, (double*)Cp, (double*)A.getDataPointer(), Astride,
            (double*)B.getDataPointer(), Bstride);
        break;
    }
    return ArrayOf(B.getDataClass(), Cdim, Cp);
}
}