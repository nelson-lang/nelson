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
#include "UminusInteger.hpp"
#include "Exception.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    ArrayOf integer_uminus(ArrayOf a)
    {
        ArrayOf R;
        if (a.isSparse())
        {
            throw Exception(_W("Type not managed in this case."));
        }
        switch (a.getDataClass())
        {
            case NLS_INT8:
            {
                int8 *pDest = (int8*)ArrayOf::allocateArrayOf(a.getDataClass(), a.getLength());
                int8 *pSrc = (int8*)a.getDataPointer();
#if defined(__NLS_WITH_OPENMP)
                #pragma omp parallel for
#endif
                for (indexType k = 0; k < a.getLength(); k++)
                {
                    pDest[k] = integer_uminus(pSrc[k]);
                }
                return ArrayOf(a.getDataClass(), a.getDimensions(), pDest, a.isSparse());
            }
            break;
            case NLS_INT16:
            {
                int16 *pDest = (int16*)ArrayOf::allocateArrayOf(a.getDataClass(), a.getLength());
                int16 *pSrc = (int16*)a.getDataPointer();
#if defined(__NLS_WITH_OPENMP)
                #pragma omp parallel for
#endif
                for (indexType k = 0; k < a.getLength(); k++)
                {
                    pDest[k] = integer_uminus(pSrc[k]);
                }
                return ArrayOf(a.getDataClass(), a.getDimensions(), pDest, a.isSparse());
            }
            break;
            case NLS_INT32:
            {
                int32 *pDest = (int32*)ArrayOf::allocateArrayOf(a.getDataClass(), a.getLength());
                int32 *pSrc = (int32*)a.getDataPointer();
#if defined(__NLS_WITH_OPENMP)
                #pragma omp parallel for
#endif
                for (indexType k = 0; k < a.getLength(); k++)
                {
                    pDest[k] = integer_uminus(pSrc[k]);
                }
                return ArrayOf(a.getDataClass(), a.getDimensions(), pDest, a.isSparse());
            }
            break;
            case NLS_INT64:
            {
                int64 *pDest = (int64*)ArrayOf::allocateArrayOf(a.getDataClass(), a.getLength());
                int64 *pSrc = (int64*)a.getDataPointer();
#if defined(__NLS_WITH_OPENMP)
                #pragma omp parallel for
#endif
                for (indexType k = 0; k < a.getLength(); k++)
                {
                    pDest[k] = integer_uminus(pSrc[k]);
                }
                return ArrayOf(a.getDataClass(), a.getDimensions(), pDest, a.isSparse());
            }
            break;
            case NLS_UINT8:
            {
                uint8 *pDest = (uint8*)ArrayOf::allocateArrayOf(a.getDataClass(), a.getLength());
                uint8 *pSrc = (uint8*)a.getDataPointer();
#if defined(__NLS_WITH_OPENMP)
                #pragma omp parallel for
#endif
                for (indexType k = 0; k < a.getLength(); k++)
                {
                    pDest[k] = integer_uminus(pSrc[k]);
                }
                return ArrayOf(a.getDataClass(), a.getDimensions(), pDest, a.isSparse());
            }
            break;
            case NLS_UINT16:
            {
                uint16 *pDest = (uint16*)ArrayOf::allocateArrayOf(a.getDataClass(), a.getLength());
                uint16 *pSrc = (uint16*)a.getDataPointer();
#if defined(__NLS_WITH_OPENMP)
                #pragma omp parallel for
#endif
                for (indexType k = 0; k < a.getLength(); k++)
                {
                    pDest[k] = integer_uminus(pSrc[k]);
                }
                return ArrayOf(a.getDataClass(), a.getDimensions(), pDest, a.isSparse());
            }
            break;
            case NLS_UINT32:
            {
                uint32 *pDest = (uint32*)ArrayOf::allocateArrayOf(a.getDataClass(), a.getLength());
                uint32 *pSrc = (uint32*)a.getDataPointer();
#if defined(__NLS_WITH_OPENMP)
                #pragma omp parallel for
#endif
                for (indexType k = 0; k < a.getLength(); k++)
                {
                    pDest[k] = integer_uminus(pSrc[k]);
                }
                return ArrayOf(a.getDataClass(), a.getDimensions(), pDest, a.isSparse());
            }
            break;
            case NLS_UINT64:
            {
                uint64 *pDest = (uint64*)ArrayOf::allocateArrayOf(a.getDataClass(), a.getLength());
                uint64 *pSrc = (uint64*)a.getDataPointer();
#if defined(__NLS_WITH_OPENMP)
                #pragma omp parallel for
#endif
                for (indexType k = 0; k < a.getLength(); k++)
                {
                    pDest[k] = integer_uminus(pSrc[k]);
                }
                return ArrayOf(a.getDataClass(), a.getDimensions(), pDest, a.isSparse());
            }
            break;
            default:
            {
                throw Exception(_W("Type not managed in this case."));
            }
            break;
        }
        return R;
    }
    //=============================================================================
    int8 integer_uminus(int8 a)
    {
        return -a;
    }
    //=============================================================================
    int16 integer_uminus(int16 a)
    {
        return -a;
    }
    //=============================================================================
    int32 integer_uminus(int32 a)
    {
        return -a;
    }
    //=============================================================================
    int64 integer_uminus(int64 a)
    {
        return -a;
    }
    //=============================================================================
    uint8 integer_uminus(uint8 a)
    {
        return 0;
    }
    //=============================================================================
    uint16 integer_uminus(uint16 a)
    {
        return 0;
    }
    //=============================================================================
    uint32 integer_uminus(uint32 a)
    {
        return 0;
    }
    //=============================================================================
    uint64 integer_uminus(uint64 a)
    {
        return 0;
    }
    //=============================================================================
}
//=============================================================================
