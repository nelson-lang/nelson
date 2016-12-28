//=============================================================================
// Copyright (c) 2016-2017 Allan CORNET (Nelson)
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
//=============================================================================
namespace Nelson {

    template <class T>
    void notequalsfuncreal(indexType N, logical* C, const T*A, int stride1, const T*B,
                           int stride2)
    {
        indexType m, p;
        m = 0;
        p = 0;
        for (indexType i = 0; i<N; i++)
        {
            C[i] = (A[m] != B[p]) ? 1 : 0;
            m += stride1;
            p += stride2;
        }
    }

    template <class T>
    void notequalsfunccomplex(indexType N, logical* C, const T*A, int stride1,
                              const T*B, int stride2)
    {
        indexType m, p;
        m = 0;
        p = 0;
        for (indexType i = 0; i<N; i++)
        {
            C[i] = ((A[2 * m] != B[2 * p]) ||
                    (A[2 * m + 1] != B[2 * p + 1])) ? 1 : 0;
            m += stride1;
            p += stride2;
        }
    }

    ArrayOf NotEquals(ArrayOf A, ArrayOf B)
    {
        // Process the two arguments through the type check and dimension checks...
        VectorCheck(A, B, false, "~=");
        int Astride, Bstride;
        void *Cp = nullptr;
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
        //Cp = Malloc(Clen*sizeof(logical));
        Cp = new_with_exception<int32>(Clen);
        switch (B.getDataClass())
        {
            case NLS_INT32:
                notequalsfuncreal<int32>(Clen, (logical*)Cp,
                                         (int32*)A.getDataPointer(), Astride,
                                         (int32*)B.getDataPointer(), Bstride);
                break;
            case NLS_SINGLE:
                notequalsfuncreal<float>(Clen, (logical*)Cp,
                                         (float*)A.getDataPointer(), Astride,
                                         (float*)B.getDataPointer(), Bstride);
                break;
            case NLS_DOUBLE:
                notequalsfuncreal<double>(Clen, (logical*)Cp,
                                          (double*)A.getDataPointer(), Astride,
                                          (double*)B.getDataPointer(), Bstride);
                break;
            case NLS_SCOMPLEX:
                notequalsfunccomplex<float>(Clen, (logical*)Cp,
                                            (float*)A.getDataPointer(), Astride,
                                            (float*)B.getDataPointer(), Bstride);
                break;
            case NLS_DCOMPLEX:
                notequalsfunccomplex<double>(Clen, (logical*)Cp,
                                             (double*)A.getDataPointer(), Astride,
                                             (double*)B.getDataPointer(), Bstride);
                break;
        }
        return ArrayOf(NLS_LOGICAL, Cdim, Cp);
    }
}
//=============================================================================

