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
#include "ToInt32.hpp"
#include "IntegerSaturate.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    int32 ToInt32(int8 a)
    {
        return (int32)a;
    }
    //=============================================================================
    int32 ToInt32(float a)
    {
        return RealToIntX<int32, float>(a);
    }
    //=============================================================================
    int32 ToInt32(double a)
    {
        return RealToIntX<int32, double>(a);
    }
    //=============================================================================
    int32 ToInt32(uint8 a)
    {
        return (int32)a;
    }
    //=============================================================================
    int32 ToInt32(int16 a)
    {
        return (int32)a;
    }
    //=============================================================================
    int32 ToInt32(uint16 a)
    {
        return (int32)a;
    }
    //=============================================================================
    int32 ToInt32(int32 a)
    {
        return (int32)a;
    }
    //=============================================================================
    int32 ToInt32(uint32 a)
    {
        if (a > (uint32)std::numeric_limits<int32>::max())
        {
            return std::numeric_limits<int32>::max();
        }
        return (int32)a;
    }
    //=============================================================================
    int32 ToInt32(int64 a)
    {
        if (a > std::numeric_limits<int32>::max())
        {
            return std::numeric_limits<int32>::max();
        }
        if (a < std::numeric_limits<int32>::min())
        {
            return std::numeric_limits<int32>::min();
        }
        return (int32)a;
    }
    //=============================================================================
    int32 ToInt32(uint64 a)
    {
        if (a > std::numeric_limits<int32>::max())
        {
            return std::numeric_limits<int32>::max();
        }
        return (int32)a;
    }
    //=============================================================================
    ArrayOf ToInt32(ArrayOf a)
    {
        std::string destType = "int32";
        Class destClass = NLS_INT32;
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
                int32 *pDest = (int32*)ArrayOf::allocateArrayOf(destClass, a.getLength());
                logical *pSrc = (logical*)a.getDataPointer();
#if defined(__NLS_WITH_OPENMP)
                #pragma omp parallel for
#endif
                for (indexType k = 0; k < a.getLength(); k++)
                {
                    pDest[k] = ToInt32(pSrc[k]);
                }
                return ArrayOf(destClass, a.getDimensions(), pSrc, a.isSparse());
            }
            break;
            case NLS_INT8:
            {
                int32 *pDest = (int32*)ArrayOf::allocateArrayOf(destClass, a.getLength());
                int8 *pSrc = (int8*)a.getDataPointer();
#if defined(__NLS_WITH_OPENMP)
                #pragma omp parallel for
#endif
                for (indexType k = 0; k < a.getLength(); k++)
                {
                    pDest[k] = ToInt32(pSrc[k]);
                }
                return ArrayOf(destClass, a.getDimensions(), pDest, a.isSparse());
            }
            break;
            case NLS_INT16:
            {
                int32 *pDest = (int32*)ArrayOf::allocateArrayOf(destClass, a.getLength());
                int16 *pSrc = (int16*)a.getDataPointer();
#if defined(__NLS_WITH_OPENMP)
                #pragma omp parallel for
#endif
                for (indexType k = 0; k < a.getLength(); k++)
                {
                    pDest[k] = ToInt32(pSrc[k]);
                }
                return ArrayOf(destClass, a.getDimensions(), pDest, a.isSparse());
            }
            break;
            case NLS_INT32:
            {
                int32 *pDest = (int32*)ArrayOf::allocateArrayOf(destClass, a.getLength());
                int32 *pSrc = (int32*)a.getDataPointer();
#if defined(__NLS_WITH_OPENMP)
                #pragma omp parallel for
#endif
                for (indexType k = 0; k < a.getLength(); k++)
                {
                    pDest[k] = ToInt32(pSrc[k]);
                }
                return ArrayOf(destClass, a.getDimensions(), pDest, a.isSparse());
            }
            break;
            case NLS_INT64:
            {
                int32 *pDest = (int32*)ArrayOf::allocateArrayOf(destClass, a.getLength());
                int64 *pSrc = (int64*)a.getDataPointer();
#if defined(__NLS_WITH_OPENMP)
                #pragma omp parallel for
#endif
                for (indexType k = 0; k < a.getLength(); k++)
                {
                    pDest[k] = ToInt32(pSrc[k]);
                }
                return ArrayOf(destClass, a.getDimensions(), pDest, a.isSparse());
            }
            break;
            case NLS_UINT8:
            {
                int32 *pDest = (int32*)ArrayOf::allocateArrayOf(destClass, a.getLength());
                uint8 *pSrc = (uint8*)a.getDataPointer();
#if defined(__NLS_WITH_OPENMP)
                #pragma omp parallel for
#endif
                for (indexType k = 0; k < a.getLength(); k++)
                {
                    pDest[k] = ToInt32(pSrc[k]);
                }
                return ArrayOf(destClass, a.getDimensions(), pDest, a.isSparse());
            }
            break;
            case NLS_UINT16:
            {
                int32 *pDest = (int32*)ArrayOf::allocateArrayOf(destClass, a.getLength());
                uint16 *pSrc = (uint16*)a.getDataPointer();
#if defined(__NLS_WITH_OPENMP)
                #pragma omp parallel for
#endif
                for (indexType k = 0; k < a.getLength(); k++)
                {
                    pDest[k] = ToInt32(pSrc[k]);
                }
                return ArrayOf(destClass, a.getDimensions(), pDest, a.isSparse());
            }
            break;
            case NLS_UINT32:
            {
                int32 *pDest = (int32*)ArrayOf::allocateArrayOf(destClass, a.getLength());
                uint32 *pSrc = (uint32*)a.getDataPointer();
#if defined(__NLS_WITH_OPENMP)
                #pragma omp parallel for
#endif
                for (indexType k = 0; k < a.getLength(); k++)
                {
                    pDest[k] = ToInt32(pSrc[k]);
                }
                return ArrayOf(destClass, a.getDimensions(), pDest, a.isSparse());
            }
            break;
            case NLS_UINT64:
            {
                int32 *pDest = (int32*)ArrayOf::allocateArrayOf(destClass, a.getLength());
                uint64 *pSrc = (uint64*)a.getDataPointer();
#if defined(__NLS_WITH_OPENMP)
                #pragma omp parallel for
#endif
                for (indexType k = 0; k < a.getLength(); k++)
                {
                    pDest[k] = ToInt32(pSrc[k]);
                }
                return ArrayOf(destClass, a.getDimensions(), pDest, a.isSparse());
            }
            break;
            case NLS_SINGLE:
            {
                int32 *pDest = (int32*)ArrayOf::allocateArrayOf(destClass, a.getLength());
                float *pSrc = (float*)a.getDataPointer();
#if defined(__NLS_WITH_OPENMP)
                #pragma omp parallel for
#endif
                for (indexType k = 0; k < a.getLength(); k++)
                {
                    pDest[k] = ToInt32(pSrc[k]);
                }
                return ArrayOf(destClass, a.getDimensions(), pDest, a.isSparse());
            }
            break;
            case NLS_DOUBLE:
            {
                int32 *pDest = (int32*)ArrayOf::allocateArrayOf(destClass, a.getLength());
                double *pSrc = (double*)a.getDataPointer();
#if defined(__NLS_WITH_OPENMP)
                #pragma omp parallel for
#endif
                for (indexType k = 0; k < a.getLength(); k++)
                {
                    pDest[k] = ToInt32(pSrc[k]);
                }
                return ArrayOf(destClass, a.getDimensions(), pDest, a.isSparse());
            }
            break;
            case NLS_CHAR:
            {
                charType *pSrc = (charType*)a.getDataPointer();
                int32 *pDest = (int32*)ArrayOf::allocateArrayOf(destClass, a.getLength());
#if defined(__NLS_WITH_OPENMP)
                #pragma omp parallel for
#endif
                for (indexType k = 0; k < a.getLength(); k++)
                {
                    pDest[k] = ToInt32(pSrc[k]);
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
