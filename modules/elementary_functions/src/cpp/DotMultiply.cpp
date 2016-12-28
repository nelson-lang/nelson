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
#include "DotMultiply.hpp"
#include "MatrixCheck.hpp"
#include "Sparse.hpp"
//=============================================================================
namespace NelSon {

    template <class T>
    void multiplyfullreal(int N, T*C, const T*A, int stride1,
                          const T*B, int stride2)
    {
        int m, p;
        m = 0;
        p = 0;
        for (int i = 0; i<N; i++)
        {
            C[i] = A[m] * B[p];
            m += stride1;
            p += stride2;
        }
    }

    template <class T>
    void multiplyfullcomplex(int N, T*C, const T*A, int stride1,
                             const T*B, int stride2)
    {
        int m, p;
        m = 0;
        p = 0;
        for (int i = 0; i<N; i++)
        {
            C[2 * i] = A[2 * m] * B[2 * p] - A[2 * m + 1] * B[2 * p + 1];
            C[2 * i + 1] = A[2 * m] * B[2 * p + 1] + A[2 * m + 1] * B[2 * p];
            m += stride1;
            p += stride2;
        }
    }

    Array DotMultiply(Array A, Array B)
    {
        // Process the two arguments through the type check and dimension checks...
        VectorCheck(A, B, false, ".*");
        // Get a pointer to the function we ultimately need to execute
        int Astride, Bstride;
        void *Cp = NULL;
        Dimensions Cdim;
        bool sparse;
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
        //FIXME - these rules don't apply for multiplication!!
        if (A.isSparse() && B.isScalar())
        {
            sparse = true;
            B.makeDense();
            Cp = SparseScalarMultiply(A.getDataClass(),
                                      A.getSparseDataPointer(),
                                      A.getDimensionLength(0),
                                      A.getDimensionLength(1),
                                      B.getDataPointer());
        }
        else if (B.isSparse() && A.isScalar())
        {
            sparse = true;
            A.makeDense();
            Cp = SparseScalarMultiply(B.getDataClass(),
                                      B.getSparseDataPointer(),
                                      B.getDimensionLength(0),
                                      B.getDimensionLength(1),
                                      A.getDataPointer());
        }
        else
        {
            if (A.isSparse() && !B.isSparse())
            {
                A.makeDense();
            }
            if (!A.isSparse() && B.isSparse())
            {
                B.makeDense();
            }
            if (A.isSparse())
            {
                sparse = true;
                Cp = SparseSparseMultiply(A.getDataClass(),
                                          A.getSparseDataPointer(),
                                          A.getDimensionLength(0),
                                          A.getDimensionLength(1),
                                          B.getSparseDataPointer());
            }
            else
            {
                sparse = false;
                int Clen = Cdim.getElementCount();
                //Cp = Malloc(Clen*B.getElementSize());
                switch (B.getDataClass())
                {
                    case NLS_INT32:
                        Cp = new_with_exception<int32>(Clen);
                        multiplyfullreal<int32>(Clen, (int32*)Cp,
                                                (int32*)A.getDataPointer(), Astride,
                                                (int32*)B.getDataPointer(), Bstride);
                        break;
                    case NLS_DOUBLE:
                        Cp = new_with_exception<double>(Clen);
                        multiplyfullreal<double>(Clen, (double*)Cp,
                                                 (double*)A.getDataPointer(), Astride,
                                                 (double*)B.getDataPointer(), Bstride);
                        break;
                    case NLS_DCOMPLEX:
                        Cp = new_with_exception<double>(Clen * 2);
                        multiplyfullcomplex<double>(Clen, (double*)Cp,
                                                    (double*)A.getDataPointer(), Astride,
                                                    (double*)B.getDataPointer(), Bstride);
                        break;
                }
            }
        }
        return Array(B.getDataClass(), Cdim, Cp, sparse);
    }
}
//=============================================================================
