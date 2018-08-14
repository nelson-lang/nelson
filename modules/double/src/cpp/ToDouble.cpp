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
#include "ToDouble.hpp"
#include "SparseDynamicFunctions.hpp"
#include <Eigen/Dense>
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
ArrayOf
ToDouble(ArrayOf A)
{
    double* pDouble = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, A.getLength());
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matA(
        (T*)A.getDataPointer(), A.getLength(), 1);
    Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic>> matC(
        pDouble, A.getLength(), 1);
    matC = matA.template cast<double>();
    ArrayOf r = ArrayOf(NLS_DOUBLE, A.getDimensions(), pDouble, A.isSparse());
    return r;
}
//=============================================================================
ArrayOf
ToDouble(ArrayOf A)
{
    switch (A.getDataClass()) {
    case NLS_HANDLE: {
        Error(_W("Conversion to double from handle is not possible."));
    } break;
    case NLS_CELL_ARRAY: {
        Error(_W("Conversion to double from cell is not possible."));
    } break;
    case NLS_STRUCT_ARRAY: {
        if (A.getStructType() != "struct") {
            Error(_("Undefined function 'double' for input arguments of type '")
                + A.getStructType() + "'.");
        } else {
            Error(_W("Conversion to double from struct is not possible."));
        }
    } break;
    case NLS_LOGICAL: {
        if (A.isSparse()) {
            void* pDouble
                = TypeConvertSparseDynamicFunction(NLS_DOUBLE, A.getDimensions().getRows(),
                    A.getDimensions().getColumns(), A.getSparseDataPointer(), NLS_LOGICAL);
            return ArrayOf(NLS_DOUBLE, A.getDimensions(), pDouble, true);
        }
        return ToDouble<logical>(A);
    } break;
    case NLS_UINT8: {
        if (A.isSparse()) {
            Error(_W("Invalid conversion: unimplemented sparse type."));
        }
        return ToDouble<uint8>(A);
    } break;
    case NLS_INT8: {
        if (A.isSparse()) {
            Error(_W("Invalid conversion: unimplemented sparse type."));
        }
        return ToDouble<int8>(A);
    } break;
    case NLS_UINT16: {
        if (A.isSparse()) {
            Error(_W("Invalid conversion: unimplemented sparse type."));
        }
        return ToDouble<uint16>(A);
    } break;
    case NLS_INT16: {
        if (A.isSparse()) {
            Error(_W("Invalid conversion: unimplemented sparse type."));
        }
        return ToDouble<int16>(A);
    } break;
    case NLS_UINT32: {
        if (A.isSparse()) {
            Error(_W("Invalid conversion: unimplemented sparse type."));
        }
        return ToDouble<uint32>(A);
    } break;
    case NLS_INT32: {
        if (A.isSparse()) {
            Error(_W("Invalid conversion: unimplemented sparse type."));
        }
        return ToDouble<int32>(A);
    } break;
    case NLS_UINT64: {
        if (A.isSparse()) {
            Error(_W("Invalid conversion: unimplemented sparse type."));
        }
        return ToDouble<uint64>(A);
    } break;
    case NLS_INT64: {
        if (A.isSparse()) {
            Error(_W("Invalid conversion: unimplemented sparse type."));
        }
        return ToDouble<int64>(A);
    } break;
    case NLS_DCOMPLEX:
    case NLS_DOUBLE: {
        ArrayOf r(A);
        r.ensureSingleOwner();
        return r;
    } break;
    case NLS_SCOMPLEX: {
        if (A.isSparse()) {
            Error(_W("Invalid conversion: unimplemented sparse type."));
        }
        double* pDouble = (double*)ArrayOf::allocateArrayOf(NLS_SCOMPLEX, A.getLength() * 2);
        ArrayOf r = ArrayOf(NLS_SCOMPLEX, A.getDimensions(), pDouble, A.isSparse());
        float* pSingle = (float*)A.getDataPointer();
#if defined(__NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (indexType k = 0; k < A.getLength() * 2; k++) {
            pDouble[k] = (double)(pSingle[k]);
        }
        return r;
    } break;
    case NLS_SINGLE: {
        if (A.isSparse()) {
            Error(_W("Invalid conversion: unimplemented sparse type."));
        }
        return ToDouble<single>(A);
    } break;
    case NLS_CHAR: {
        if (A.isSparse()) {
            Error(_W("Invalid conversion: unimplemented sparse type."));
        }
        return ToDouble<charType>(A);
    } break;
    default: {
        if (A.isSparse()) {
            Error(_W("Invalid conversion: unimplemented sparse type."));
        }
        Error(_W("Invalid conversion: unimplemented type."));
    } break;
    }
    return ArrayOf();
}
//=============================================================================
}
//=============================================================================
