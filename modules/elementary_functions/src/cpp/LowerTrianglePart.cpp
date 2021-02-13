//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <cstring>
#include "nlsConfig.h"
#include "LowerTrianglePart.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
ArrayOf
LowerTrianglePartComplex(const ArrayOf& A, signedIndexType offset)
{
    Class classA = A.getDataClass();
    indexType nbElements = A.getElementCount();
    ArrayOf res = A;
    res.ensureSingleOwner();
    T* D = (T*)res.getDataPointer();
    auto* Dz = reinterpret_cast<std::complex<T>*>(D);
    indexType C = A.getColumns();
    indexType R = A.getRows();
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
    for (ompIndexType i = 0; i < (ompIndexType)C; i++) {
        auto v = (signedIndexType)(i - offset);
        indexType iSize = std::min(std::max(v, (signedIndexType)0), (signedIndexType)R);
        memset(&Dz[i * R], 0x00, iSize * sizeof(std::complex<T>));
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
LowerTrianglePartReal(const ArrayOf& A, signedIndexType offset)
{
    Class classA = A.getDataClass();
    ArrayOf res = A;
    res.ensureSingleOwner();
    T* D = (T*)res.getDataPointer();
    indexType C = A.getColumns();
    indexType R = A.getRows();
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
    for (ompIndexType i = 0; i < (ompIndexType)C; i++) {
        auto v = (signedIndexType)(i - offset);
        indexType iSize = std::min(std::max(v, (signedIndexType)0), (signedIndexType)R);
        memset(&D[i * R], 0x00, iSize * sizeof(T));
    }
    return res;
}
//=============================================================================
ArrayOf
LowerTrianglePart(const ArrayOf& A, signedIndexType offset, bool& needToOverload)
{
    ArrayOf res;
    needToOverload = false;
    if (!(A.is2D() || A.isVector() || A.isEmpty())) {
        Error(_W("Vector or matrix 2D expected."));
    }
    switch (A.getDataClass()) {
    case NLS_LOGICAL: {
        if (A.isSparse()) {
            needToOverload = true;
            return ArrayOf();
        }
        res = LowerTrianglePartReal<logical>(A, offset);

    } break;
    case NLS_CHAR: {
        res = LowerTrianglePartReal<charType>(A, offset);
    } break;
    case NLS_UINT8: {
        res = LowerTrianglePartReal<uint8>(A, offset);
    } break;
    case NLS_UINT16: {
        res = LowerTrianglePartReal<uint16>(A, offset);
    } break;
    case NLS_UINT32: {
        res = LowerTrianglePartReal<uint32>(A, offset);
    } break;
    case NLS_UINT64: {
        res = LowerTrianglePartReal<uint64>(A, offset);
    } break;
    case NLS_INT8: {
        res = LowerTrianglePartReal<int8>(A, offset);
    } break;
    case NLS_INT16: {
        res = LowerTrianglePartReal<int16>(A, offset);
    } break;
    case NLS_INT32: {
        res = LowerTrianglePartReal<int32>(A, offset);
    } break;
    case NLS_INT64: {
        res = LowerTrianglePartReal<int64>(A, offset);
    } break;
    case NLS_SINGLE: {
        res = LowerTrianglePartReal<single>(A, offset);
    } break;
    case NLS_DOUBLE: {
        if (A.isSparse()) {
            needToOverload = true;
            return ArrayOf();
        }
        res = LowerTrianglePartReal<double>(A, offset);

    } break;
    case NLS_SCOMPLEX: {
        res = LowerTrianglePartComplex<single>(A, offset);
    } break;
    case NLS_DCOMPLEX: {
        if (A.isSparse()) {
            needToOverload = true;
            return ArrayOf();
        }
        res = LowerTrianglePartComplex<double>(A, offset);
    } break;
    default: {
        needToOverload = true;
        return ArrayOf();
    } break;
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
