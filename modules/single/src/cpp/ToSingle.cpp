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
#include <Eigen/Dense>
#include "ToSingle.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    template <class T> ArrayOf ToSingle(ArrayOf A)
    {
        single *pSingle = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, A.getLength());
        Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matA((T*)A.getDataPointer(), A.getLength(), 1);
        Eigen::Map<Eigen::Matrix<single, Eigen::Dynamic, Eigen::Dynamic>> matC(pSingle, A.getLength(), 1);
        matC = matA.template cast<single>();
        ArrayOf r = ArrayOf(NLS_SINGLE, A.getDimensions(), pSingle, A.isSparse());
        return r;
    }
    //=============================================================================
    ArrayOf ToSingle(ArrayOf A)
    {
        switch (A.getDataClass())
        {
            case NLS_HANDLE:
            {
                throw Exception(_W("Conversion to single from handle is not possible."));
            }
            break;
            case NLS_CELL_ARRAY:
            {
                throw Exception(_W("Conversion to single from cell is not possible."));
            }
            break;
            case NLS_STRUCT_ARRAY:
            {
                if (A.getStructType() != "struct")
                {
                    throw Exception(_("Undefined function 'single' for input arguments of type '") + A.getStructType() + "'.");
                }
                else
                {
                    throw Exception(_W("Conversion to single from struct is not possible."));
                }
            }
            break;
            case NLS_LOGICAL:
            {
                if (A.isSparse())
                {
                    throw Exception(_W("Invalid conversion: unimplemented sparse type."));
                }
                return ToSingle<logical>(A);
            }
            break;
            case NLS_UINT8:
            {
                if (A.isSparse())
                {
                    throw Exception(_W("Invalid conversion: unimplemented sparse type."));
                }
                return ToSingle<uint8>(A);
            }
            break;
            case NLS_INT8:
            {
                if (A.isSparse())
                {
                    throw Exception(_W("Invalid conversion: unimplemented sparse type."));
                }
                return ToSingle<int8>(A);
            }
            break;
            case NLS_UINT16:
            {
                if (A.isSparse())
                {
                    throw Exception(_W("Invalid conversion: unimplemented sparse type."));
                }
                return ToSingle<uint16>(A);
            }
            break;
            case NLS_INT16:
            {
                if (A.isSparse())
                {
                    throw Exception(_W("Invalid conversion: unimplemented sparse type."));
                }
                return ToSingle<int16>(A);
            }
            break;
            case NLS_UINT32:
            {
                if (A.isSparse())
                {
                    throw Exception(_W("Invalid conversion: unimplemented sparse type."));
                }
                return ToSingle<uint32>(A);
            }
            break;
            case NLS_INT32:
            {
                if (A.isSparse())
                {
                    throw Exception(_W("Invalid conversion: unimplemented sparse type."));
                }
                return ToSingle<int32>(A);
            }
            break;
            case NLS_UINT64:
            {
                if (A.isSparse())
                {
                    throw Exception(_W("Invalid conversion: unimplemented sparse type."));
                }
                return ToSingle<uint64>(A);
            }
            break;
            case NLS_INT64:
            {
                if (A.isSparse())
                {
                    throw Exception(_W("Invalid conversion: unimplemented sparse type."));
                }
                return ToSingle<int16>(A);
            }
            break;
            case NLS_SCOMPLEX:
            case NLS_SINGLE:
            {
                ArrayOf r(A);
                r.ensureSingleOwner();
                return r;
            }
            break;
            case NLS_DCOMPLEX:
            {
                if (A.isSparse())
                {
                    throw Exception(_W("Invalid conversion: unimplemented sparse type."));
                }
                float *pSingle = (float*)ArrayOf::allocateArrayOf(NLS_SCOMPLEX, A.getLength() * 2);
                ArrayOf r = ArrayOf(NLS_SCOMPLEX, A.getDimensions(), pSingle, A.isSparse());
                double *pDouble = (double*)A.getDataPointer();
#if defined(__NLS_WITH_OPENMP)
                #pragma omp parallel for
#endif
                for (indexType k = 0; k < A.getLength() * 2; k++)
                {
                    pSingle[k] = (float)(pDouble[k]);
                }
                return r;
            }
            break;
            case NLS_DOUBLE:
            {
                if (A.isSparse())
                {
                    throw Exception(_W("Invalid conversion: unimplemented sparse type."));
                }
                return ToSingle<double>(A);
            }
            break;
            case NLS_CHAR:
            {
                if (A.isSparse())
                {
                    throw Exception(_W("Invalid conversion: unimplemented sparse type."));
                }
                return ToSingle<charType>(A);
            }
            break;
            default:
            {
                if (A.isSparse())
                {
                    throw Exception(_W("Invalid conversion: unimplemented sparse type."));
                }
                throw Exception(_W("Invalid conversion: unimplemented type."));
            }
            break;
        }
        return ArrayOf();
    }
    //=============================================================================
}
//=============================================================================
