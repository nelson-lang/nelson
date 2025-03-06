//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cstring>
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "UpperTrianglePart.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
ArrayOf
UpperTrianglePartComplex(const ArrayOf& A, signedIndexType offset)
{
    NelsonType classA = A.getDataClass();
    Dimensions dimsA = A.getDimensions();
    indexType nbElements = dimsA.getElementCount();
    T* D = (T*)ArrayOf::allocateArrayOf(classA, nbElements, stringVector(), true);
    auto* Dz = reinterpret_cast<std::complex<T>*>(D);
    T* S = (T*)A.getDataPointer();
    auto* Sz = reinterpret_cast<std::complex<T>*>(S);
    indexType C = dimsA.getColumns();
    indexType R = dimsA.getRows();
    OMP_PARALLEL_FOR_LOOP(C)
    for (ompIndexType i = 0; i < (ompIndexType)C; i++) {
        auto v = (signedIndexType)(i + 1 - offset);
        indexType iSize = std::min(std::max(v, (signedIndexType)0), (signedIndexType)R);
        memcpy(&Dz[i * R], &Sz[i * R], iSize * sizeof(std::complex<T>));
    }
    return ArrayOf(classA, dimsA, D);
}
//=============================================================================

template <class T>
ArrayOf
UpperTrianglePartReal(const ArrayOf& A, signedIndexType offset)
{
    NelsonType classA = A.getDataClass();
    Dimensions dimsA = A.getDimensions();
    indexType nbElements = dimsA.getElementCount();
    T* D = (T*)ArrayOf::allocateArrayOf(classA, nbElements, stringVector(), true);
    T* S = (T*)A.getDataPointer();
    indexType C = dimsA.getColumns();
    indexType R = dimsA.getRows();
    OMP_PARALLEL_FOR_LOOP(C)
    for (ompIndexType i = 0; i < (ompIndexType)C; i++) {
        auto v = (signedIndexType)(i + 1 - offset);
        indexType iSize = std::min(std::max(v, (signedIndexType)0), (signedIndexType)R);
        memcpy(&D[i * R], &S[i * R], iSize * sizeof(T));
    }
    return ArrayOf(classA, dimsA, D);
}
//=============================================================================
ArrayOf
UpperTrianglePart(const ArrayOf& A, signedIndexType offset, bool& needToOverload)
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
        res = UpperTrianglePartReal<logical>(A, offset);

    } break;
    case NLS_CHAR: {
        res = UpperTrianglePartReal<charType>(A, offset);
    } break;
    case NLS_UINT8: {
        res = UpperTrianglePartReal<uint8>(A, offset);
    } break;
    case NLS_UINT16: {
        res = UpperTrianglePartReal<uint16>(A, offset);
    } break;
    case NLS_UINT32: {
        res = UpperTrianglePartReal<uint32>(A, offset);
    } break;
    case NLS_UINT64: {
        res = UpperTrianglePartReal<uint64>(A, offset);
    } break;
    case NLS_INT8: {
        res = UpperTrianglePartReal<int8>(A, offset);
    } break;
    case NLS_INT16: {
        res = UpperTrianglePartReal<int16>(A, offset);
    } break;
    case NLS_INT32: {
        res = UpperTrianglePartReal<int32>(A, offset);
    } break;
    case NLS_INT64: {
        res = UpperTrianglePartReal<int64>(A, offset);
    } break;
    case NLS_SINGLE: {
        res = UpperTrianglePartReal<single>(A, offset);
    } break;
    case NLS_DOUBLE: {
        if (A.isSparse()) {
            needToOverload = true;
            return {};
        }
        res = UpperTrianglePartReal<double>(A, offset);

    } break;
    case NLS_SCOMPLEX: {
        res = UpperTrianglePartComplex<single>(A, offset);
    } break;
    case NLS_DCOMPLEX: {
        if (A.isSparse()) {
            needToOverload = true;
            return {};
        }
        res = UpperTrianglePartComplex<double>(A, offset);

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
