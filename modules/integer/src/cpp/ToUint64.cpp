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
#include "ToUint64.hpp"
#include "IntegerSaturate.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    uint64 ToUint64(int8 a)
    {
        if (a > std::numeric_limits<uint64>::max())
        {
            return std::numeric_limits<uint64>::max();
        }
        if (a < std::numeric_limits<uint64>::min())
        {
            return std::numeric_limits<uint64>::min();
        }
        return (uint64)a;
    }
    //=============================================================================
    uint64 ToUint64(float a)
    {
        return RealToIntX<uint64, float>(a);
    }
    //=============================================================================
    uint64 ToUint64(double a)
    {
        return RealToIntX<uint64, double>(a);
    }
    //=============================================================================
    uint64 ToUint64(uint8 a)
    {
        return (uint64)a;
    }
    //=============================================================================
    uint64 ToUint64(int16 a)
    {
        if (a > std::numeric_limits<uint64>::max())
        {
            return std::numeric_limits<uint64>::max();
        }
        if (a < std::numeric_limits<uint64>::min())
        {
            return std::numeric_limits<uint64>::min();
        }
        return (uint64)a;
    }
    //=============================================================================
    uint64 ToUint64(uint16 a)
    {
        return (uint64)a;
    }
    //=============================================================================
    uint64 ToUint64(int32 a)
    {
        if (a > std::numeric_limits<uint64>::max())
        {
            return std::numeric_limits<uint64>::max();
        }
        if (a < std::numeric_limits<uint64>::min())
        {
            return std::numeric_limits<uint64>::min();
        }
        return (uint64)a;
    }
    //=============================================================================
    uint64 ToUint64(uint32 a)
    {
        return (uint64)a;
    }
    //=============================================================================
    uint64 ToUint64(int64 a)
    {
        if (a > (int64)std::numeric_limits<uint64>::max())
        {
            return std::numeric_limits<uint64>::max();
        }
        if (a < (int64)std::numeric_limits<uint64>::min())
        {
            return std::numeric_limits<uint64>::min();
        }
        return (uint64)a;
    }
    //=============================================================================
    uint64 ToUint64(uint64 a)
    {
        return (uint64)a;
    }
    //=============================================================================
    ArrayOf ToUint64(ArrayOf a)
    {
        std::string destType = "uint64";
        Class destClass = NLS_UINT64;
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
                uint64 *pDest = (uint64*)ArrayOf::allocateArrayOf(destClass, a.getLength());
                logical *pSrc = (logical*)a.getDataPointer();
#if defined(__NLS_WITH_OPENMP)
                #pragma omp parallel for
#endif
                for (indexType k = 0; k < a.getLength(); k++)
                {
                    pDest[k] = ToUint64(pSrc[k]);
                }
                return ArrayOf(destClass, a.getDimensions(), pSrc, a.isSparse());
            }
            break;
            case NLS_INT8:
            {
                uint64 *pDest = (uint64*)ArrayOf::allocateArrayOf(destClass, a.getLength());
                int8 *pSrc = (int8*)a.getDataPointer();
#if defined(__NLS_WITH_OPENMP)
                #pragma omp parallel for
#endif
                for (indexType k = 0; k < a.getLength(); k++)
                {
                    pDest[k] = ToUint64(pSrc[k]);
                }
                return ArrayOf(destClass, a.getDimensions(), pDest, a.isSparse());
            }
            break;
            case NLS_INT16:
            {
                uint64 *pDest = (uint64*)ArrayOf::allocateArrayOf(destClass, a.getLength());
                int16 *pSrc = (int16*)a.getDataPointer();
#if defined(__NLS_WITH_OPENMP)
                #pragma omp parallel for
#endif
                for (indexType k = 0; k < a.getLength(); k++)
                {
                    pDest[k] = ToUint64(pSrc[k]);
                }
                return ArrayOf(destClass, a.getDimensions(), pDest, a.isSparse());
            }
            break;
            case NLS_INT32:
            {
                uint64 *pDest = (uint64*)ArrayOf::allocateArrayOf(destClass, a.getLength());
                int32 *pSrc = (int32*)a.getDataPointer();
#if defined(__NLS_WITH_OPENMP)
                #pragma omp parallel for
#endif
                for (indexType k = 0; k < a.getLength(); k++)
                {
                    pDest[k] = ToUint64(pSrc[k]);
                }
                return ArrayOf(destClass, a.getDimensions(), pDest, a.isSparse());
            }
            break;
            case NLS_INT64:
            {
                uint64 *pDest = (uint64*)ArrayOf::allocateArrayOf(destClass, a.getLength());
                int64 *pSrc = (int64*)a.getDataPointer();
#if defined(__NLS_WITH_OPENMP)
                #pragma omp parallel for
#endif
                for (indexType k = 0; k < a.getLength(); k++)
                {
                    pDest[k] = ToUint64(pSrc[k]);
                }
                return ArrayOf(destClass, a.getDimensions(), pDest, a.isSparse());
            }
            break;
            case NLS_UINT8:
            {
                uint64 *pDest = (uint64*)ArrayOf::allocateArrayOf(destClass, a.getLength());
                uint8 *pSrc = (uint8*)a.getDataPointer();
#if defined(__NLS_WITH_OPENMP)
                #pragma omp parallel for
#endif
                for (indexType k = 0; k < a.getLength(); k++)
                {
                    pDest[k] = ToUint64(pSrc[k]);
                }
                return ArrayOf(destClass, a.getDimensions(), pDest, a.isSparse());
            }
            break;
            case NLS_UINT16:
            {
                uint64 *pDest = (uint64*)ArrayOf::allocateArrayOf(destClass, a.getLength());
                uint16 *pSrc = (uint16*)a.getDataPointer();
#if defined(__NLS_WITH_OPENMP)
                #pragma omp parallel for
#endif
                for (indexType k = 0; k < a.getLength(); k++)
                {
                    pDest[k] = ToUint64(pSrc[k]);
                }
                return ArrayOf(destClass, a.getDimensions(), pDest, a.isSparse());
            }
            break;
            case NLS_UINT32:
            {
                uint64 *pDest = (uint64*)ArrayOf::allocateArrayOf(destClass, a.getLength());
                uint32 *pSrc = (uint32*)a.getDataPointer();
#if defined(__NLS_WITH_OPENMP)
                #pragma omp parallel for
#endif
                for (indexType k = 0; k < a.getLength(); k++)
                {
                    pDest[k] = ToUint64(pSrc[k]);
                }
                return ArrayOf(destClass, a.getDimensions(), pDest, a.isSparse());
            }
            break;
            case NLS_UINT64:
            {
                uint64 *pDest = (uint64*)ArrayOf::allocateArrayOf(destClass, a.getLength());
                uint64 *pSrc = (uint64*)a.getDataPointer();
#if defined(__NLS_WITH_OPENMP)
                #pragma omp parallel for
#endif
                for (indexType k = 0; k < a.getLength(); k++)
                {
                    pDest[k] = ToUint64(pSrc[k]);
                }
                return ArrayOf(destClass, a.getDimensions(), pDest, a.isSparse());
            }
            break;
            case NLS_SINGLE:
            {
                uint64 *pDest = (uint64*)ArrayOf::allocateArrayOf(destClass, a.getLength());
                float *pSrc = (float*)a.getDataPointer();
#if defined(__NLS_WITH_OPENMP)
                #pragma omp parallel for
#endif
                for (indexType k = 0; k < a.getLength(); k++)
                {
                    pDest[k] = ToUint64(pSrc[k]);
                }
                return ArrayOf(destClass, a.getDimensions(), pDest, a.isSparse());
            }
            break;
            case NLS_DOUBLE:
            {
                uint64 *pDest = (uint64*)ArrayOf::allocateArrayOf(destClass, a.getLength());
                double *pSrc = (double*)a.getDataPointer();
#if defined(__NLS_WITH_OPENMP)
                #pragma omp parallel for
#endif
                for (indexType k = 0; k < a.getLength(); k++)
                {
                    pDest[k] = ToUint64(pSrc[k]);
                }
                return ArrayOf(destClass, a.getDimensions(), pDest, a.isSparse());
            }
            break;
            case NLS_STRING:
            {
                charType *pSrc = (charType*)a.getDataPointer();
                uint64 *pDest = (uint64*)ArrayOf::allocateArrayOf(destClass, a.getLength());
#if defined(__NLS_WITH_OPENMP)
                #pragma omp parallel for
#endif
                for (indexType k = 0; k < a.getLength(); k++)
                {
                    pDest[k] = ToUint64(pSrc[k]);
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
