//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cstdint>
#include <type_traits>
#include "SwapBytes.hpp"
#include "Error.hpp"
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
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
inline uint16_t
bswap16(uint16_t x) noexcept
{
    return static_cast<uint16_t>((x >> 8) | (x << 8));
}
//=============================================================================
inline uint32_t
bswap32(uint32_t x) noexcept
{
    return ((x >> 24) & 0x000000FFu) | ((x >> 8) & 0x0000FF00u) | ((x << 8) & 0x00FF0000u)
        | ((x << 24) & 0xFF000000u);
}
//=============================================================================
inline uint64_t
bswap64(uint64_t x) noexcept
{
    return ((x >> 56) & 0x00000000000000FFULL) | ((x >> 40) & 0x000000000000FF00ULL)
        | ((x >> 24) & 0x0000000000FF0000ULL) | ((x >> 8) & 0x00000000FF000000ULL)
        | ((x << 8) & 0x000000FF00000000ULL) | ((x << 24) & 0x0000FF0000000000ULL)
        | ((x << 40) & 0x00FF000000000000ULL) | ((x << 56) & 0xFF00000000000000ULL);
}
//=============================================================================
template <typename T>
inline std::enable_if_t<sizeof(T) == 1, void>
endian_reverse_inplace(T&)
{
    // no-op for 1-byte types
}
//=============================================================================
template <typename T>
inline std::enable_if_t<sizeof(T) == 2, void>
endian_reverse_inplace(T& v)
{
    using U = std::make_unsigned_t<T>;
    U uv = static_cast<U>(v);
    uv = bswap16(static_cast<uint16_t>(uv));
    v = static_cast<T>(uv);
}
//=============================================================================
template <typename T>
inline std::enable_if_t<sizeof(T) == 4, void>
endian_reverse_inplace(T& v)
{
    using U = std::make_unsigned_t<T>;
    U uv = static_cast<U>(v);
    uv = bswap32(static_cast<uint32_t>(uv));
    v = static_cast<T>(uv);
}
//=============================================================================
template <typename T>
inline std::enable_if_t<sizeof(T) == 8, void>
endian_reverse_inplace(T& v)
{
    using U = std::make_unsigned_t<T>;
    U uv = static_cast<U>(v);
    uv = bswap64(static_cast<uint64_t>(uv));
    v = static_cast<T>(uv);
}
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
        OMP_PARALLEL_FOR_LOOP(elementCount)
        for (ompIndexType k = 0; k < elementCount; ++k) {
            endian_reverse_inplace(ptr[k]);
        }
    } break;
    case NLS_INT8: {
        res = A;
        res.ensureSingleOwner();
        int8* ptr = (int8*)res.getDataPointer();
        ompIndexType elementCount = dimsA.getElementCount();
        OMP_PARALLEL_FOR_LOOP(elementCount)
        for (ompIndexType k = 0; k < elementCount; ++k) {
            endian_reverse_inplace(ptr[k]);
        }
    } break;
    case NLS_UINT16: {
        res = A;
        res.ensureSingleOwner();
        auto* ptr = (uint16*)res.getDataPointer();
        ompIndexType elementCount = dimsA.getElementCount();
        OMP_PARALLEL_FOR_LOOP(elementCount)
        for (ompIndexType k = 0; k < elementCount; ++k) {
            endian_reverse_inplace(ptr[k]);
        }
    } break;
    case NLS_INT16: {
        res = A;
        res.ensureSingleOwner();
        auto* ptr = (int16*)res.getDataPointer();
        ompIndexType elementCount = dimsA.getElementCount();
        OMP_PARALLEL_FOR_LOOP(elementCount)
        for (ompIndexType k = 0; k < elementCount; ++k) {
            endian_reverse_inplace(ptr[k]);
        }
    } break;
    case NLS_UINT32: {
        res = A;
        res.ensureSingleOwner();
        auto* ptr = (uint32*)res.getDataPointer();
        ompIndexType elementCount = dimsA.getElementCount();
        OMP_PARALLEL_FOR_LOOP(elementCount)
        for (ompIndexType k = 0; k < elementCount; ++k) {
            endian_reverse_inplace(ptr[k]);
        }
    } break;
    case NLS_INT32: {
        res = A;
        res.ensureSingleOwner();
        auto* ptr = (int32*)res.getDataPointer();
        ompIndexType elementCount = dimsA.getElementCount();
        OMP_PARALLEL_FOR_LOOP(elementCount)
        for (ompIndexType k = 0; k < elementCount; ++k) {
            endian_reverse_inplace(ptr[k]);
        }
    } break;
    case NLS_UINT64: {
        res = A;
        res.ensureSingleOwner();
        auto* ptr = (uint64*)res.getDataPointer();
        ompIndexType elementCount = dimsA.getElementCount();
        OMP_PARALLEL_FOR_LOOP(elementCount)
        for (ompIndexType k = 0; k < elementCount; ++k) {
            endian_reverse_inplace(ptr[k]);
        }
    } break;
    case NLS_INT64: {
        res = A;
        res.ensureSingleOwner();
        auto* ptr = (int64*)res.getDataPointer();
        ompIndexType elementCount = dimsA.getElementCount();
        OMP_PARALLEL_FOR_LOOP(elementCount)
        for (ompIndexType k = 0; k < elementCount; ++k) {
            endian_reverse_inplace(ptr[k]);
        }
    } break;
    case NLS_SINGLE: {
        res = A;
        res.ensureSingleOwner();
        auto* ptr = (single*)res.getDataPointer();
        ompIndexType elementCount = dimsA.getElementCount();
        OMP_PARALLEL_FOR_LOOP(elementCount)
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
            OMP_PARALLEL_FOR_LOOP(elementCount)
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
