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
#include "ToLogical.hpp"
#include "SparseDynamicFunctions.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
ToLogical(ArrayOf A)
{
    switch (A.getDataClass()) {
    case NLS_LOGICAL: {
        ArrayOf r(A);
        r.ensureSingleOwner();
        return r;
    } break;
    case NLS_HANDLE: {
        Error(_W("Conversion to logical from handle is not possible."));
    } break;
    case NLS_STRING_ARRAY: {
        Error(_W("Conversion to logical from string is not possible."));
    } break;
    case NLS_CELL_ARRAY: {
        Error(_W("Conversion to logical from cell is not possible."));
    } break;
    case NLS_STRUCT_ARRAY: {
        if (A.getStructType() != "struct") {
            Error(_("Undefined function 'logical' for input arguments of type '")
                + A.getStructType() + "'.");
        } else {
            Error(_W("Conversion to logical from struct is not possible."));
        }
    } break;
    case NLS_DCOMPLEX:
    case NLS_SCOMPLEX: {
        Error(_W("Conversion to logical from complex is not possible."));
    } break;
    case NLS_DOUBLE: {
        if (A.isSparse()) {
            void* pLogical
                = TypeConvertSparseDynamicFunction(NLS_LOGICAL, A.getDimensions().getRows(),
                    A.getDimensions().getColumns(), A.getSparseDataPointer(), NLS_DOUBLE);
            return ArrayOf(NLS_LOGICAL, A.getDimensions(), pLogical, true);
        } else {
            logical* pLogical = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, A.getLength());
            ArrayOf r = ArrayOf(NLS_LOGICAL, A.getDimensions(), pLogical, false);
            double* pDouble = (double*)A.getDataPointer();
#if defined(__NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
            for (indexType k = 0; k < A.getLength(); k++) {
                if (std::isnan(pDouble[k])) {
                    Error(_W("Conversion to logical with NaN is not possible."));
                }
                pLogical[k] = (logical)(pDouble[k] != 0.0);
            }
            return r;
        }
    } break;
    case NLS_SINGLE: {
        if (A.isSparse()) {
            Error(_W("Conversion to logical from single is not possible."));
        } else {
            logical* pLogical = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, A.getLength());
            ArrayOf r = ArrayOf(NLS_LOGICAL, A.getDimensions(), pLogical, false);
            float* pSingle = (float*)A.getDataPointer();
#if defined(__NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
            for (indexType k = 0; k < A.getLength(); k++) {
                if (std::isnan(pSingle[k])) {
                    Error(_W("Conversion to logical with NaN is not possible."));
                }
                pLogical[k] = (logical)(pSingle[k] != 0.0);
            }
            return r;
        }
    } break;
    case NLS_UINT8: {
        if (A.isSparse()) {
            Error(_W("Conversion to logical from sparse uint8 is not possible."));
        } else {
            logical* pLogical = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, A.getLength());
            ArrayOf r = ArrayOf(NLS_LOGICAL, A.getDimensions(), pLogical, false);
            uint8* pUint8 = (uint8*)A.getDataPointer();
#if defined(__NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
            for (indexType k = 0; k < A.getLength(); k++) {
                pLogical[k] = (logical)(pUint8[k] != 0);
            }
            return r;
        }
    } break;
    case NLS_INT8: {
        if (A.isSparse()) {
            Error(_W("Conversion to logical from sparse int8 is not possible."));
        } else {
            logical* pLogical = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, A.getLength());
            ArrayOf r = ArrayOf(NLS_LOGICAL, A.getDimensions(), pLogical, false);
            int8* pInt8 = (int8*)A.getDataPointer();
#if defined(__NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
            for (indexType k = 0; k < A.getLength(); k++) {
                pLogical[k] = (logical)(pInt8[k] != 0);
            }
            return r;
        }
    } break;
    case NLS_UINT16: {
        if (A.isSparse()) {
            Error(_W("Conversion to logical from sparse uint16 is not possible."));
        } else {
            logical* pLogical = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, A.getLength());
            ArrayOf r = ArrayOf(NLS_LOGICAL, A.getDimensions(), pLogical, false);
            uint16* pUint16 = (uint16*)A.getDataPointer();
#if defined(__NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
            for (indexType k = 0; k < A.getLength(); k++) {
                pLogical[k] = (logical)(pUint16[k] != 0);
            }
            return r;
        }
    } break;
    case NLS_INT16: {
        if (A.isSparse()) {
            Error(_W("Conversion to logical from sparse int16 is not possible."));
        } else {
            logical* pLogical = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, A.getLength());
            ArrayOf r = ArrayOf(NLS_LOGICAL, A.getDimensions(), pLogical, false);
            int16* pInt16 = (int16*)A.getDataPointer();
#if defined(__NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
            for (indexType k = 0; k < A.getLength(); k++) {
                pLogical[k] = (logical)(pInt16[k] != 0);
            }
            return r;
        }
    } break;
    case NLS_UINT32: {
        if (A.isSparse()) {
            Error(_W("Conversion to logical from sparse uint32 is not possible."));
        } else {
            logical* pLogical = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, A.getLength());
            ArrayOf r = ArrayOf(NLS_LOGICAL, A.getDimensions(), pLogical, false);
            uint32* pUint32 = (uint32*)A.getDataPointer();
#if defined(__NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
            for (indexType k = 0; k < A.getLength(); k++) {
                pLogical[k] = (logical)(pUint32[k] != 0);
            }
            return r;
        }
    } break;
    case NLS_INT32: {
        if (A.isSparse()) {
            Error(_W("Conversion to logical from sparse int32 is not possible."));
        } else {
            logical* pLogical = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, A.getLength());
            ArrayOf r = ArrayOf(NLS_LOGICAL, A.getDimensions(), pLogical, false);
            int32* pInt32 = (int32*)A.getDataPointer();
#if defined(__NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
            for (indexType k = 0; k < A.getLength(); k++) {
                pLogical[k] = (logical)(pInt32[k] != 0);
            }
            return r;
        }
    } break;
    case NLS_UINT64: {
        if (A.isSparse()) {
            Error(_W("Conversion to logical from sparse uint64 is not possible."));
        } else {
            logical* pLogical = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, A.getLength());
            ArrayOf r = ArrayOf(NLS_LOGICAL, A.getDimensions(), pLogical, false);
            uint64* pUint64 = (uint64*)A.getDataPointer();
#if defined(__NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
            for (indexType k = 0; k < A.getLength(); k++) {
                pLogical[k] = (logical)(pUint64[k] != 0);
            }
            return r;
        }
    } break;
    case NLS_INT64: {
        if (A.isSparse()) {
            Error(_W("Conversion to logical from sparse int64 is not possible."));
        } else {
            logical* pLogical = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, A.getLength());
            ArrayOf r = ArrayOf(NLS_LOGICAL, A.getDimensions(), pLogical, false);
            int64* pInt64 = (int64*)A.getDataPointer();
#if defined(__NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
            for (indexType k = 0; k < A.getLength(); k++) {
                pLogical[k] = (logical)(pInt64[k] != 0);
            }
            return r;
        }
    } break;
    default: {
        Error(_W("Invalid conversion."));
    } break;
    }
    return ArrayOf();
}
//=============================================================================
}
//=============================================================================
