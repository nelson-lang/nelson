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
#include "MatrixCheck.hpp"
//=============================================================================
namespace Nelson {
    template <class T>
    void greaterthanfuncreal(indexType N, logical* C, const T*A, int stride1, const T*B,
                             int stride2)
    {
        indexType m, p;
        m = 0;
        p = 0;
        for (indexType i = 0; i<N; i++)
        {
            C[i] = (A[m] > B[p]) ? 1 : 0;
            m += stride1;
            p += stride2;
        }
    }


    template <class T>
    T complex_abs(T real, T imag)
    {
        double temp;
        if (real < 0)
        {
            real = -real;
        }
        if (imag < 0)
        {
            imag = -imag;
        }
        if (imag > real)
        {
            temp = real;
            real = imag;
            imag = (T)(temp);
        }
        if ((real + imag) == real)
        {
            return(real);
        }
        temp = imag / real;
        temp = real*sqrt(1.0 + temp*temp);  /*overflow!!*/
        return (T)(temp);
    }


    template <class T>
    void greaterthanfunccomplex(indexType N, logical* C, const T*A, int stride1,
                                const T*B, int stride2)
    {
        indexType m, p;
        m = 0;
        p = 0;
        for (indexType i = 0; i<N; i++)
        {
            C[i] = (complex_abs<T>(A[2 * m], A[2 * m + 1]) >
                    complex_abs<T>(B[2 * p], B[2 * p + 1])) ? 1 : 0;
            m += stride1;
            p += stride2;
        }
    }

    ArrayOf GreaterThan(ArrayOf A, ArrayOf B)
    {
        // Process the two arguments through the type check and dimension checks...
        VectorCheck(A, B, ">");
        int Astride, Bstride;
        indexType Clen = 0;
        Dimensions Cdim;
        if (A.isScalar())
        {
            Astride = 0;
            Bstride = 1;
            Cdim = B.getDimensions();
        }
        else if (B.isScalar())
        {
            Astride = 1;
            Bstride = 0;
            Cdim = A.getDimensions();
        }
        else
        {
            Astride = 1;
            Bstride = 1;
            Cdim = A.getDimensions();
        }
        Clen = Cdim.getElementCount();
        void *Cp = new_with_exception<logical>(Clen);
        switch (B.getDataClass())
        {
            case NLS_INT32:
                greaterthanfuncreal<int32>(Clen, (logical*)Cp,
                                           (int32*)A.getDataPointer(), Astride,
                                           (int32*)B.getDataPointer(), Bstride);
                break;
            case NLS_SINGLE:
                greaterthanfuncreal<float>(Clen, (logical*)Cp,
                                           (float*)A.getDataPointer(), Astride,
                                           (float*)B.getDataPointer(), Bstride);
                break;
            case NLS_DOUBLE:
                greaterthanfuncreal<double>(Clen, (logical*)Cp,
                                            (double*)A.getDataPointer(), Astride,
                                            (double*)B.getDataPointer(), Bstride);
                break;
            case NLS_SCOMPLEX:
                greaterthanfunccomplex<float>(Clen, (logical*)Cp,
                                              (float*)A.getDataPointer(), Astride,
                                              (float*)B.getDataPointer(), Bstride);
                break;
            case NLS_DCOMPLEX:
                greaterthanfunccomplex<double>(Clen, (logical*)Cp,
                                               (double*)A.getDataPointer(), Astride,
                                               (double*)B.getDataPointer(), Bstride);
                break;
        }
        return ArrayOf(NLS_LOGICAL, Cdim, Cp);
    }
}
//=============================================================================
