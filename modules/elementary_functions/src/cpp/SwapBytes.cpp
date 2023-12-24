//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <boost/endian/conversion.hpp>
#include "SwapBytes.hpp"
#include "Error.hpp"
#include "nlsBuildConfig.h"
//=============================================================================
namespace Nelson {
//=============================================================================
#define num_swap(a, b)                                                                             \
    do {                                                                                           \
        (a) ^= (b);                                                                                \
        (b) ^= (a);                                                                                \
        (a) ^= (b);                                                                                \
    } while (0)
//=============================================================================
static double
swapDouble(double a)
{
    auto* raw = reinterpret_cast<uint8*>(&a);
    num_swap(raw[0], raw[7]);
    num_swap(raw[1], raw[6]);
    num_swap(raw[2], raw[5]);
    num_swap(raw[3], raw[4]);
    return a;
}
//=============================================================================
static single
swapSingle(single a)
{
    auto* raw = reinterpret_cast<uint8*>(&a);
    num_swap(raw[0], raw[3]);
    num_swap(raw[1], raw[2]);
    return a;
}
//=============================================================================
ArrayOf
SwapBytes(const ArrayOf& A, bool& needToOverload)
{
    ArrayOf res;
    needToOverload = false;
    Dimensions dimsA = A.getDimensions();
    switch (A.getDataClass()) {
    case NLS_UINT8: {
        res = A;
        res.ensureSingleOwner();
        auto* ptr = (uint8*)res.getDataPointer();
        ompIndexType elementCount = dimsA.getElementCount();
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < elementCount; ++k) {
            boost::endian::endian_reverse_inplace(ptr[k]);
        }
    } break;
    case NLS_INT8: {
        res = A;
        res.ensureSingleOwner();
        int8* ptr = (int8*)res.getDataPointer();
        ompIndexType elementCount = dimsA.getElementCount();
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < elementCount; ++k) {
            boost::endian::endian_reverse_inplace(ptr[k]);
        }
    } break;
    case NLS_UINT16: {
        res = A;
        res.ensureSingleOwner();
        auto* ptr = (uint16*)res.getDataPointer();
        ompIndexType elementCount = dimsA.getElementCount();
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < elementCount; ++k) {
            boost::endian::endian_reverse_inplace(ptr[k]);
        }
    } break;
    case NLS_INT16: {
        res = A;
        res.ensureSingleOwner();
        auto* ptr = (int16*)res.getDataPointer();
        ompIndexType elementCount = dimsA.getElementCount();
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < elementCount; ++k) {
            boost::endian::endian_reverse_inplace(ptr[k]);
        }
    } break;
    case NLS_UINT32: {
        res = A;
        res.ensureSingleOwner();
        auto* ptr = (uint32*)res.getDataPointer();
        ompIndexType elementCount = dimsA.getElementCount();
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < elementCount; ++k) {
            boost::endian::endian_reverse_inplace(ptr[k]);
        }
    } break;
    case NLS_INT32: {
        res = A;
        res.ensureSingleOwner();
        auto* ptr = (int32*)res.getDataPointer();
        ompIndexType elementCount = dimsA.getElementCount();
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < elementCount; ++k) {
            boost::endian::endian_reverse_inplace(ptr[k]);
        }
    } break;
    case NLS_UINT64: {
        res = A;
        res.ensureSingleOwner();
        auto* ptr = (uint64*)res.getDataPointer();
        ompIndexType elementCount = dimsA.getElementCount();
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < elementCount; ++k) {
            boost::endian::endian_reverse_inplace(ptr[k]);
        }
    } break;
    case NLS_INT64: {
        res = A;
        res.ensureSingleOwner();
        auto* ptr = (int64*)res.getDataPointer();
        ompIndexType elementCount = dimsA.getElementCount();
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < elementCount; ++k) {
            boost::endian::endian_reverse_inplace(ptr[k]);
        }
    } break;
    case NLS_SINGLE: {
        res = A;
        res.ensureSingleOwner();
        auto* ptr = (single*)res.getDataPointer();
        ompIndexType elementCount = dimsA.getElementCount();
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < elementCount; ++k) {
            ptr[k] = swapSingle(ptr[k]);
        }
    } break;
    case NLS_DOUBLE: {
        if (A.isSparse()) {
            needToOverload = true;
        } else {
            res = A;
            res.ensureSingleOwner();
            auto* ptr = (double*)res.getDataPointer();
            ompIndexType elementCount = dimsA.getElementCount();
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
            for (ompIndexType k = 0; k < elementCount; ++k) {
                ptr[k] = swapDouble(ptr[k]);
            }
        }
    } break;
    case NLS_CHAR:
    case NLS_LOGICAL:
    case NLS_SCOMPLEX:
    case NLS_DCOMPLEX:
    default: {
        needToOverload = true;
    } break;
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
