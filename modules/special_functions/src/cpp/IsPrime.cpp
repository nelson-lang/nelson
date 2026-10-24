//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cmath>
#include <cstdint>
#include <type_traits>
#if defined(_MSC_VER) && defined(_M_X64)
#include <intrin.h>
#endif
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "IsPrime.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
isPrimeUInt64(uint64_t n);
template <class T>
static bool
isNonnegativeIntegerValue(T value);
template <class T>
static uint64_t
toUInt64(T value);
template <class T>
static ArrayOf
isPrimeTyped(const ArrayOf& A);
//=============================================================================
ArrayOf
IsPrime(const ArrayOf& A)
{
    ArrayOf X(A);
    if (X.isSparse()) {
        X.makeDense();
    }
    switch (X.dataClass()) {
    case NLS_DOUBLE:
        return isPrimeTyped<double>(X);
    case NLS_SINGLE:
        return isPrimeTyped<single>(X);
    case NLS_DCOMPLEX:
    case NLS_SCOMPLEX: {
        if (!X.allReal()) {
            Error(L"Nelson:isprime:nonNonnegativeInteger",
                L"Input must contain nonnegative integers.");
        }
        X.promoteType(X.dataClass() == NLS_DCOMPLEX ? NLS_DOUBLE : NLS_SINGLE);
        return IsPrime(X);
    }
    case NLS_INT8:
        return isPrimeTyped<int8>(X);
    case NLS_UINT8:
        return isPrimeTyped<uint8>(X);
    case NLS_INT16:
        return isPrimeTyped<int16>(X);
    case NLS_UINT16:
        return isPrimeTyped<uint16>(X);
    case NLS_INT32:
        return isPrimeTyped<int32>(X);
    case NLS_UINT32:
        return isPrimeTyped<uint32>(X);
    case NLS_INT64:
        return isPrimeTyped<int64>(X);
    case NLS_UINT64:
        return isPrimeTyped<uint64>(X);
    case NLS_LOGICAL:
    case NLS_CHAR: {
        Error(L"Nelson:isprime:nonNonnegativeInteger", L"Input must contain nonnegative integers.");
    } break;
    default: {
        Error(L"Nelson:isprime:nonNumericInput", L"Input must be numeric.");
    } break;
    }
    return {};
}
//=============================================================================
template <class T>
static ArrayOf
isPrimeTyped(const ArrayOf& A)
{
    Dimensions dims = A.dimensions();
    indexType N = dims.elementCount();
    const T* src = static_cast<const T*>(A.dataPointer());
    for (indexType i = 0; i < N; ++i) {
        if (!isNonnegativeIntegerValue<T>(src[i])) {
            Error(L"Nelson:isprime:nonNonnegativeInteger",
                L"Input must contain nonnegative integers.");
        }
    }
    logical* dst = static_cast<logical*>(ArrayOf::allocateArrayOf(NLS_LOGICAL, N));
    OMP_PARALLEL_FOR_LOOP(N)
    for (ompIndexType i = 0; i < (ompIndexType)N; ++i) {
        dst[i] = isPrimeUInt64(toUInt64<T>(src[i])) ? 1 : 0;
    }
    return ArrayOf(NLS_LOGICAL, dims, dst);
}
//=============================================================================
template <class T>
static bool
isNonnegativeIntegerValue(T value)
{
    if constexpr (std::is_floating_point_v<T>) {
        return std::isfinite(value) && value >= (T)0 && std::trunc(value) == value;
    } else if constexpr (std::is_signed_v<T>) {
        return value >= (T)0;
    } else {
        return true;
    }
}
//=============================================================================
template <class T>
static uint64_t
toUInt64(T value)
{
    if constexpr (std::is_floating_point_v<T>) {
        // values at or above 2^64 are even multiples of a power of two: not prime
        if (value >= (T)18446744073709551616.0) {
            return 0;
        }
        return static_cast<uint64_t>(value);
    } else {
        return static_cast<uint64_t>(value);
    }
}
//=============================================================================
static inline uint64_t
addModulo(uint64_t a, uint64_t b, uint64_t m)
{
    // a, b < m
    return (a >= m - b) ? a - (m - b) : a + b;
}
//=============================================================================
static inline uint64_t
mulModulo(uint64_t a, uint64_t b, uint64_t m)
{
#if defined(__SIZEOF_INT128__)
    return static_cast<uint64_t>((static_cast<unsigned __int128>(a) * b) % m);
#elif defined(_MSC_VER) && defined(_M_X64)
    uint64_t high = 0;
    uint64_t low = _umul128(a, b, &high);
    uint64_t remainder = 0;
    _udiv128(high, low, m, &remainder);
    return remainder;
#else
    uint64_t result = 0;
    a %= m;
    while (b != 0) {
        if (b & 1) {
            result = addModulo(result, a, m);
        }
        a = addModulo(a, a, m);
        b >>= 1;
    }
    return result;
#endif
}
//=============================================================================
static inline uint64_t
powModulo(uint64_t base, uint64_t exponent, uint64_t m)
{
    uint64_t result = 1;
    base %= m;
    while (exponent != 0) {
        if (exponent & 1) {
            result = mulModulo(result, base, m);
        }
        base = mulModulo(base, base, m);
        exponent >>= 1;
    }
    return result;
}
//=============================================================================
// deterministic Miller-Rabin test for the given witness bases
static bool
millerRabin(uint64_t n, const uint64_t* bases, size_t nbBases)
{
    uint64_t d = n - 1;
    int s = 0;
    while ((d & 1) == 0) {
        d >>= 1;
        s++;
    }
    for (size_t k = 0; k < nbBases; ++k) {
        uint64_t a = bases[k] % n;
        if (a == 0) {
            continue;
        }
        uint64_t x = powModulo(a, d, n);
        if (x == 1 || x == n - 1) {
            continue;
        }
        bool composite = true;
        for (int r = 1; r < s; ++r) {
            x = mulModulo(x, x, n);
            if (x == n - 1) {
                composite = false;
                break;
            }
        }
        if (composite) {
            return false;
        }
    }
    return true;
}
//=============================================================================
static bool
isPrimeUInt64(uint64_t n)
{
    static const uint64_t smallPrimes[] = { 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47,
        53, 59, 61, 67, 71, 73, 79, 83, 89, 97 };
    if (n < 2) {
        return false;
    }
    for (uint64_t p : smallPrimes) {
        if (n == p) {
            return true;
        }
        if (n % p == 0) {
            return false;
        }
    }
    if (n < 101 * 101) {
        return true;
    }
    // witness sets proven sufficient below 2^32 and below 2^64
    static const uint64_t bases32[] = { 2, 7, 61 };
    static const uint64_t bases64[] = { 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37 };
    if (n < 4294967296ULL) {
        return millerRabin(n, bases32, sizeof(bases32) / sizeof(bases32[0]));
    }
    return millerRabin(n, bases64, sizeof(bases64) / sizeof(bases64[0]));
}
//=============================================================================
} // namespace Nelson
//=============================================================================
