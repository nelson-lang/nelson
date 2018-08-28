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
#include "ToSingle.hpp"
#include <Eigen/Dense>
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
ArrayOf
ToSingle(const ArrayOf& A)
{
    single* pSingle = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, A.getLength());
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matA(
        (T*)A.getDataPointer(), A.getLength(), 1);
    Eigen::Map<Eigen::Matrix<single, Eigen::Dynamic, Eigen::Dynamic>> matC(
        pSingle, A.getLength(), 1);
    matC = matA.template cast<single>();
    ArrayOf r = ArrayOf(NLS_SINGLE, A.getDimensions(), pSingle, A.isSparse());
    return r;
}
//=============================================================================
ArrayOf
ToSingle(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    if (A.isSparse()) {
        needToOverload = true;
        return ArrayOf();
    }
    switch (A.getDataClass()) {
    case NLS_LOGICAL: {
        return ToSingle<logical>(A);
    } break;
    case NLS_UINT8: {
        return ToSingle<uint8>(A);
    } break;
    case NLS_INT8: {
        return ToSingle<int8>(A);
    } break;
    case NLS_UINT16: {
        return ToSingle<uint16>(A);
    } break;
    case NLS_INT16: {
        return ToSingle<int16>(A);
    } break;
    case NLS_UINT32: {
        return ToSingle<uint32>(A);
    } break;
    case NLS_INT32: {
        return ToSingle<int32>(A);
    } break;
    case NLS_UINT64: {
        return ToSingle<uint64>(A);
    } break;
    case NLS_INT64: {
        return ToSingle<int16>(A);
    } break;
    case NLS_SCOMPLEX:
    case NLS_SINGLE: {
        ArrayOf r(A);
        r.ensureSingleOwner();
        return r;
    } break;
    case NLS_DCOMPLEX: {
        float* pSingle = (float*)ArrayOf::allocateArrayOf(NLS_SCOMPLEX, A.getLength() * 2);
        ArrayOf r = ArrayOf(NLS_SCOMPLEX, A.getDimensions(), pSingle, A.isSparse());
        double* pDouble = (double*)A.getDataPointer();
#if defined(__NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (indexType k = 0; k < A.getLength() * 2; k++) {
            pSingle[k] = (float)(pDouble[k]);
        }
        return r;
    } break;
    case NLS_DOUBLE: {
        return ToSingle<double>(A);
    } break;
    case NLS_CHAR: {
        return ToSingle<charType>(A);
    } break;
    default: {
        needToOverload = true;
    } break;
    }
    return ArrayOf();
}
//=============================================================================
}
//=============================================================================
