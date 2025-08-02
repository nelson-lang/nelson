//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ToSingle.hpp"
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
ArrayOf
ToSingle(const ArrayOf& A)
{
    ompIndexType nbElements = A.getElementCount();
    auto* pSingle = static_cast<single*>(const_cast<void*>(static_cast<const void*>(
        ArrayOf::allocateArrayOf(NLS_SINGLE, nbElements, stringVector(), false))));
    ArrayOf r = ArrayOf(NLS_SINGLE, A.getDimensions(), pSingle, A.isSparse());
    T* ptrA = static_cast<T*>(const_cast<void*>(static_cast<const void*>(A.getDataPointer())));
    if (nbElements == 1) {
        pSingle[0] = static_cast<single>(ptrA[0]);
        return r;
    }

    OMP_PARALLEL_FOR_LOOP(nbElements)
    for (ompIndexType i = 0; i < nbElements; ++i) {
        pSingle[i] = static_cast<single>(ptrA[i]);
    }
    return r;
}
//=============================================================================
ArrayOf
ToSingle(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    if (A.isSparse()) {
        needToOverload = true;
        return {};
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
        return ToSingle<int64>(A);
    } break;
    case NLS_SCOMPLEX:
    case NLS_SINGLE: {
        ArrayOf r(A);
        r.ensureSingleOwner();
        return r;
    } break;
    case NLS_DCOMPLEX: {
        ompIndexType nbElements = A.getElementCount();
        single* pSingle
            = (single*)ArrayOf::allocateArrayOf(NLS_SCOMPLEX, nbElements, stringVector(), false);
        ArrayOf r = ArrayOf(NLS_SCOMPLEX, A.getDimensions(), pSingle, false);
        auto* pDouble = (double*)A.getDataPointer();
        OMP_PARALLEL_FOR_LOOP(nbElements)
        for (ompIndexType k = 0; k < (ompIndexType)(nbElements * 2); k++) {
            pSingle[k] = static_cast<single>(pDouble[k]);
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
    return {};
}
//=============================================================================
} // namespace Nelson
//=============================================================================
