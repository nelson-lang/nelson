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
#include "Equals.hpp"
#include "MatrixCheck.hpp"
#include "Exception.hpp"
#include "ClassName.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    template <class T>
    void equalsReal(indexType N, logical* C, const T*A, int stride1, const T*B, int stride2)
    {
        if ((stride1 == 1) && (stride2 == 1))
        {
#if defined(__NLS_WITH_OPENMP)
            #pragma omp parallel for
#endif
            for (indexType i = 0; i < N; i++)
            {
                C[i] = (A[i] == B[i]) ? logical(1) : logical(0);
            }
        }
        else
        {
            if (stride1)
            {
#if defined(__NLS_WITH_OPENMP)
                #pragma omp parallel for
#endif
                for (indexType i = 0; i < N; i++)
                {
                    C[i] = (A[i] == B[0]) ? logical(1) : logical(0);
                }
            }
            else
            {
#if defined(__NLS_WITH_OPENMP)
                #pragma omp parallel for
#endif
                for (indexType i = 0; i < N; i++)
                {
                    C[i] = (A[0] == B[i]) ? logical(1) : logical(0);
                }
            }
        }
    }
    //=============================================================================
    template <class T>
    void equalsComplex(indexType N, logical* C, const T*A, int stride1, const T*B, int stride2)
    {
        if ((stride1 == 1) && (stride2 == 1))
        {
#if defined(__NLS_WITH_OPENMP)
            #pragma omp parallel for
#endif
            for (indexType i = 0; i < N; i++)
            {
                C[i] = ((A[2 * i] == B[2 * i]) && (A[2 * i + 1] == B[2 * i + 1])) ? logical(1) : logical(0);
            }
        }
        else
        {
            if (stride1 == 1)
            {
#if defined(__NLS_WITH_OPENMP)
                #pragma omp parallel for
#endif
                for (indexType i = 0; i < N; i++)
                {
                    C[i] = ((A[2 * i] == B[0]) && (A[2 * i + 1] == B[1])) ? logical(1) : logical(0);
                }
            }
            else
            {
#if defined(__NLS_WITH_OPENMP)
                #pragma omp parallel for
#endif
                for (indexType i = 0; i < N; i++)
                {
                    C[i] = ((A[0] == B[2 * i]) && (A[1] == B[2 * i + 1])) ? logical(1) : logical(0);
                }
            }
        }
    }
    //=============================================================================
    ArrayOf Equals(ArrayOf &A, ArrayOf &B, bool mustRaiseError, bool &bSuccess)
    {
		if (A.isSparse() || B.isSparse())
		{
			if (mustRaiseError)
			{
				std::string overload = ClassName(A) + "_plus_" + ClassName(B);
				throw Exception(_("function") + " " + overload + " " + _("undefined."));
			}
			else
			{
				bSuccess = false;
				return ArrayOf();
			}
		}
		Class classCommon = FindCommonType(A, B, false);
        try
        {
            A.promoteType(classCommon);
            B.promoteType(classCommon);
        }
        catch (Exception)
        {
            if (mustRaiseError)
            {
				throw;
			}
            else
            {
                bSuccess = false;
                return ArrayOf();
            }
        }
		VectorCheck(A, B, "==");
		int Astride = 0, Bstride = 0;
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
            case NLS_LOGICAL:
            {
                equalsReal<logical>(Clen, (logical*)Cp, (logical*)A.getDataPointer(), Astride, (logical*)B.getDataPointer(), Bstride);
            }
            break;
            case NLS_UINT8:
            {
                equalsReal<uint8>(Clen, (logical*)Cp, (uint8*)A.getDataPointer(), Astride, (uint8*)B.getDataPointer(), Bstride);
            }
            break;
            case NLS_INT8:
            {
                equalsReal<int8>(Clen, (logical*)Cp, (int8*)A.getDataPointer(), Astride, (int8*)B.getDataPointer(), Bstride);
            }
            break;
            case NLS_UINT16:
            {
                equalsReal<uint16>(Clen, (logical*)Cp, (uint16*)A.getDataPointer(), Astride, (uint16*)B.getDataPointer(), Bstride);
            }
            break;
            case NLS_INT16:
            {
                equalsReal<int16>(Clen, (logical*)Cp, (int16*)A.getDataPointer(), Astride, (int16*)B.getDataPointer(), Bstride);
            }
            break;
            case NLS_UINT32:
            {
                equalsReal<uint32>(Clen, (logical*)Cp, (uint32*)A.getDataPointer(), Astride, (uint32*)B.getDataPointer(), Bstride);
            }
            break;
            case NLS_INT32:
            {
                equalsReal<int32>(Clen, (logical*)Cp, (int32*)A.getDataPointer(), Astride, (int32*)B.getDataPointer(), Bstride);
            }
            break;
            case NLS_UINT64:
            {
                equalsReal<uint64>(Clen, (logical*)Cp, (uint64*)A.getDataPointer(), Astride, (uint64*)B.getDataPointer(), Bstride);
            }
            break;
            case NLS_INT64:
            {
                equalsReal<int64>(Clen, (logical*)Cp, (int64*)A.getDataPointer(), Astride, (int64*)B.getDataPointer(), Bstride);
            }
            break;
            case NLS_SINGLE:
            {
                equalsReal<float>(Clen, (logical*)Cp, (float*)A.getDataPointer(), Astride, (float*)B.getDataPointer(), Bstride);
            }
            break;
            case NLS_DOUBLE:
            {
                equalsReal<double>(Clen, (logical*)Cp, (double*)A.getDataPointer(), Astride, (double*)B.getDataPointer(), Bstride);
            }
            break;
            case NLS_SCOMPLEX:
            {
                equalsComplex<float>(Clen, (logical*)Cp, (float*)A.getDataPointer(), Astride, (float*)B.getDataPointer(), Bstride);
            }
            break;
            case NLS_DCOMPLEX:
            {
                equalsComplex<double>(Clen, (logical*)Cp, (double*)A.getDataPointer(), Astride, (double*)B.getDataPointer(), Bstride);
            }
            break;
            case NLS_CHAR:
            {
                equalsReal<charType>(Clen, (logical*)Cp, (charType*)A.getDataPointer(), Astride, (charType*)B.getDataPointer(), Bstride);
            }
            break;
            default:
            {
                if (mustRaiseError)
                {
					std::string overload = ClassName(A) + "_eq_" + ClassName(B);
					throw Exception(_("function") + " " + overload + " " + _("undefined."));
                }
                else
                {
                    bSuccess = false;
                    return ArrayOf();
                }
            }
            break;
        }
        bSuccess = true;
        return ArrayOf(NLS_LOGICAL, Cdim, Cp);
    }
    //=============================================================================
}
//=============================================================================
