//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cstring>
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "LowerTrianglePart.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
ArrayOf
LowerTrianglePartComplex(const ArrayOf& A, signedIndexType offset)
{
    Dimensions dimsA = A.getDimensions();
    indexType nbElements = dimsA.getElementCount();
    ArrayOf res = A;
    res.ensureSingleOwner();
    T* D = (T*)res.getDataPointer();
    auto* Dz = reinterpret_cast<std::complex<T>*>(D);
    indexType C = dimsA.getColumns();
    indexType R = dimsA.getRows();
    OMP_PARALLEL_FOR_LOOP(C)
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
    Dimensions dimsA = A.getDimensions();
    ArrayOf res = A;
    res.ensureSingleOwner();
    T* D = (T*)res.getDataPointer();
    indexType C = dimsA.getColumns();
    indexType R = dimsA.getRows();
    OMP_PARALLEL_FOR_LOOP(C)
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
    Dimensions dimsA = A.getDimensions();
    switch (A.getDataClass()) {
    case NLS_LOGICAL: {
        if (A.isSparse()) {
            needToOverload = true;
            return {};
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
            return {};
        }
        res = LowerTrianglePartReal<double>(A, offset);

    } break;
    case NLS_SCOMPLEX: {
        res = LowerTrianglePartComplex<single>(A, offset);
    } break;
    case NLS_DCOMPLEX: {
        if (A.isSparse()) {
            needToOverload = true;
            return {};
        }
        res = LowerTrianglePartComplex<double>(A, offset);
    } break;
    default: {
        needToOverload = true;
        return {};
    } break;
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
