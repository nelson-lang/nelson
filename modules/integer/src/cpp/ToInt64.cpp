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
#include "ToInt64.hpp"
#include "IntegerSaturate.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    int64 ToInt64(int8 a)
    {
        return (int64)a;
    }
    //=============================================================================
    int64 ToInt64(float a)
    {
        return RealToIntX<int64, float>(a);
    }
    //=============================================================================
    int64 ToInt64(double a)
    {
        return RealToIntX<int64, double>(a);
    }
    //=============================================================================
    int64 ToInt64(uint8 a)
    {
        return (int64)a;
    }
    //=============================================================================
    int64 ToInt64(int16 a)
    {
        return (int64)a;
    }
    //=============================================================================
    int64 ToInt64(uint16 a)
    {
        return (int64)a;
    }
    //=============================================================================
    int64 ToInt64(int32 a)
    {
        return (int64)a;
    }
    //=============================================================================
    int64 ToInt64(uint32 a)
    {
        return (int64)a;
    }
    //=============================================================================
    int64 ToInt64(int64 a)
    {
        return (int64)a;
    }
    //=============================================================================
    int64 ToInt64(uint64 a)
    {
        if (a > (uint64)std::numeric_limits<int64>::max())
        {
            return std::numeric_limits<int64>::max();
        }
        return (int64)a;
    }
    //=============================================================================
    ArrayOf ToInt64(ArrayOf a)
    {
        std::string destType = "int64";
        Class destClass = NLS_INT64;
        if (a.isSparse())
        {
            throw Exception(_("Conversion to '") + destType + _("' from sparse matrix is not possible."));
        }
        switch (a.getDataClass())
        {
            case NLS_DCOMPLEX:
            case NLS_SCOMPLEX:
            {
                throw Exception(_("Invalid conversion from complex matrix to '") + destType + _("' matrix."));
            }
            break;
            case NLS_HANDLE:
            {
                throw Exception(_("Conversion to '") + destType + _("' from handle is not possible."));
            }
            break;
            case NLS_CELL_ARRAY:
            {
                throw Exception(_("Conversion to '") + destType + _("' from cell is not possible."));
            }
            break;
            case NLS_STRUCT_ARRAY:
            {
                if (a.getStructType() != "struct")
                {
                    throw Exception(_("Undefined function '") + destType + _("' for input arguments of type '") + a.getStructType() + "'.");
                }
                else
                {
                    throw Exception(_("Conversion to '") + destType + _("' from struct is not possible."));
                }
            }
            break;
            case NLS_LOGICAL:
            {
                int64 *pDest = (int64*)ArrayOf::allocateArrayOf(destClass, a.getLength());
                logical *pSrc = (logical*)a.getDataPointer();
#if defined(__NLS_WITH_OPENMP)
                #pragma omp parallel for
#endif
                for (indexType k = 0; k < a.getLength(); k++)
                {
                    pDest[k] = ToInt64(pSrc[k]);
                }
                return ArrayOf(destClass, a.getDimensions(), pSrc, a.isSparse());
            }
            break;
            case NLS_INT8:
            {
                int64 *pDest = (int64*)ArrayOf::allocateArrayOf(destClass, a.getLength());
                int8 *pSrc = (int8*)a.getDataPointer();
#if defined(__NLS_WITH_OPENMP)
                #pragma omp parallel for
#endif
                for (indexType k = 0; k < a.getLength(); k++)
                {
                    pDest[k] = ToInt64(pSrc[k]);
                }
                return ArrayOf(destClass, a.getDimensions(), pDest, a.isSparse());
            }
            break;
            case NLS_INT16:
            {
                int64 *pDest = (int64*)ArrayOf::allocateArrayOf(destClass, a.getLength());
                int16 *pSrc = (int16*)a.getDataPointer();
#if defined(__NLS_WITH_OPENMP)
                #pragma omp parallel for
#endif
                for (indexType k = 0; k < a.getLength(); k++)
                {
                    pDest[k] = ToInt64(pSrc[k]);
                }
                return ArrayOf(destClass, a.getDimensions(), pDest, a.isSparse());
            }
            break;
            case NLS_INT32:
            {
                int64 *pDest = (int64*)ArrayOf::allocateArrayOf(destClass, a.getLength());
                int32 *pSrc = (int32*)a.getDataPointer();
#if defined(__NLS_WITH_OPENMP)
                #pragma omp parallel for
#endif
                for (indexType k = 0; k < a.getLength(); k++)
                {
                    pDest[k] = ToInt64(pSrc[k]);
                }
                return ArrayOf(destClass, a.getDimensions(), pDest, a.isSparse());
            }
            break;
            case NLS_INT64:
            {
                int64 *pDest = (int64*)ArrayOf::allocateArrayOf(destClass, a.getLength());
                int64 *pSrc = (int64*)a.getDataPointer();
#if defined(__NLS_WITH_OPENMP)
                #pragma omp parallel for
#endif
                for (indexType k = 0; k < a.getLength(); k++)
                {
                    pDest[k] = ToInt64(pSrc[k]);
                }
                return ArrayOf(destClass, a.getDimensions(), pDest, a.isSparse());
            }
            break;
            case NLS_UINT8:
            {
                int64 *pDest = (int64*)ArrayOf::allocateArrayOf(destClass, a.getLength());
                uint8 *pSrc = (uint8*)a.getDataPointer();
#if defined(__NLS_WITH_OPENMP)
                #pragma omp parallel for
#endif
                for (indexType k = 0; k < a.getLength(); k++)
                {
                    pDest[k] = ToInt64(pSrc[k]);
                }
                return ArrayOf(destClass, a.getDimensions(), pDest, a.isSparse());
            }
            break;
            case NLS_UINT16:
            {
                int64 *pDest = (int64*)ArrayOf::allocateArrayOf(destClass, a.getLength());
                uint16 *pSrc = (uint16*)a.getDataPointer();
#if defined(__NLS_WITH_OPENMP)
                #pragma omp parallel for
#endif
                for (indexType k = 0; k < a.getLength(); k++)
                {
                    pDest[k] = ToInt64(pSrc[k]);
                }
                return ArrayOf(destClass, a.getDimensions(), pDest, a.isSparse());
            }
            break;
            case NLS_UINT32:
            {
                int64 *pDest = (int64*)ArrayOf::allocateArrayOf(destClass, a.getLength());
                uint32 *pSrc = (uint32*)a.getDataPointer();
#if defined(__NLS_WITH_OPENMP)
                #pragma omp parallel for
#endif
                for (indexType k = 0; k < a.getLength(); k++)
                {
                    pDest[k] = ToInt64(pSrc[k]);
                }
                return ArrayOf(destClass, a.getDimensions(), pDest, a.isSparse());
            }
            break;
            case NLS_UINT64:
            {
                int64 *pDest = (int64*)ArrayOf::allocateArrayOf(destClass, a.getLength());
                uint64 *pSrc = (uint64*)a.getDataPointer();
#if defined(__NLS_WITH_OPENMP)
                #pragma omp parallel for
#endif
                for (indexType k = 0; k < a.getLength(); k++)
                {
                    pDest[k] = ToInt64(pSrc[k]);
                }
                return ArrayOf(destClass, a.getDimensions(), pDest, a.isSparse());
            }
            break;
            case NLS_SINGLE:
            {
                int64 *pDest = (int64*)ArrayOf::allocateArrayOf(destClass, a.getLength());
                float *pSrc = (float*)a.getDataPointer();
#if defined(__NLS_WITH_OPENMP)
                #pragma omp parallel for
#endif
                for (indexType k = 0; k < a.getLength(); k++)
                {
                    pDest[k] = ToInt64(pSrc[k]);
                }
                return ArrayOf(destClass, a.getDimensions(), pDest, a.isSparse());
            }
            break;
            case NLS_DOUBLE:
            {
                int64 *pDest = (int64*)ArrayOf::allocateArrayOf(destClass, a.getLength());
                double *pSrc = (double*)a.getDataPointer();
#if defined(__NLS_WITH_OPENMP)
                #pragma omp parallel for
#endif
                for (indexType k = 0; k < a.getLength(); k++)
                {
                    pDest[k] = ToInt64(pSrc[k]);
                }
                return ArrayOf(destClass, a.getDimensions(), pDest, a.isSparse());
            }
            break;
            case NLS_STRING:
            {
                charType *pSrc = (charType*)a.getDataPointer();
                int64 *pDest = (int64*)ArrayOf::allocateArrayOf(destClass, a.getLength());
#if defined(__NLS_WITH_OPENMP)
                #pragma omp parallel for
#endif
                for (indexType k = 0; k < a.getLength(); k++)
                {
                    pDest[k] = ToInt64(pSrc[k]);
                }
                return ArrayOf(destClass, a.getDimensions(), pDest, a.isSparse());
            }
            break;
            default:
            {
                throw Exception(_W("Invalid conversion."));
            }
            break;
        }
        return ArrayOf();
    }
    //=============================================================================
}
//=============================================================================
