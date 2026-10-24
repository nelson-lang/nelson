//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#if defined(_MSC_VER) && (defined(_M_X64) || defined(_M_ARM64))
#include <intrin.h>
#endif
#include "Random64BitEngine.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// PCG64 DXSM (O'Neill): 128-bit linear congruential state advanced with the
// 64-bit "cheap multiplier", output by the double xor-shift multiply permutation
// of the state before the step. The odd increment selects the stream.
// Words: [state high, state low, increment high, increment low].
struct Pcg64DxsmEngine
{
    static constexpr size_t N = 4;
    static constexpr uint64 CHEAP_MULTIPLIER = 0xDA942042E4DD58B5ULL;
    //=============================================================================
    static inline uint64
    mulHigh(uint64 a, uint64 b)
    {
#if defined(_MSC_VER) && (defined(_M_X64) || defined(_M_ARM64))
        return __umulh(a, b);
#elif defined(__SIZEOF_INT128__)
        return static_cast<uint64>((static_cast<unsigned __int128>(a) * b) >> 64);
#else
        const uint64 aLo = a & 0xFFFFFFFFULL, aHi = a >> 32;
        const uint64 bLo = b & 0xFFFFFFFFULL, bHi = b >> 32;
        const uint64 lolo = aLo * bLo, hilo = aHi * bLo, lohi = aLo * bHi;
        const uint64 cross = (lolo >> 32) + (hilo & 0xFFFFFFFFULL) + lohi;
        return aHi * bHi + (hilo >> 32) + (cross >> 32);
#endif
    }
    //=============================================================================
    static inline void
    step(uint64* w)
    {
        const uint64 lo = w[1] * CHEAP_MULTIPLIER;
        const uint64 hi = w[0] * CHEAP_MULTIPLIER + mulHigh(w[1], CHEAP_MULTIPLIER);
        const uint64 newLo = lo + w[3];
        w[0] = hi + w[2] + (newLo < lo ? 1 : 0);
        w[1] = newLo;
    }
    //=============================================================================
    static inline uint64
    next(uint64* w)
    {
        uint64 hi = w[0];
        const uint64 lo = w[1] | 1;
        hi ^= hi >> 32;
        hi *= CHEAP_MULTIPLIER;
        hi ^= hi >> 48;
        hi *= lo;
        step(w);
        return hi;
    }
    //=============================================================================
    // Same initialization as setseq-style seeding: state 0,
    // increment (sequence << 1) | 1, step, add the initial state, step.
    static void
    seed(uint64* w, uint64 seedValue)
    {
        uint64 x = seedValue;
        const uint64 stateHi = splitMix64Next(x);
        const uint64 stateLo = splitMix64Next(x);
        const uint64 seqHi = splitMix64Next(x);
        const uint64 seqLo = splitMix64Next(x);
        w[0] = 0;
        w[1] = 0;
        w[2] = (seqHi << 1) | (seqLo >> 63);
        w[3] = (seqLo << 1) | 1;
        step(w);
        const uint64 lo = w[1] + stateLo;
        w[0] = w[0] + stateHi + (lo < stateLo ? 1 : 0);
        w[1] = lo;
        step(w);
    }
    //=============================================================================
    // the increment must be odd.
    static bool
    normalize(uint64* w)
    {
        return (w[3] & 1) != 0;
    }
    //=============================================================================
};
//=============================================================================
class RandomPcg64Dxsm : public Random64BitEngine<Pcg64DxsmEngine>
{
public:
    std::wstring
    getGeneratorName() override
    {
        return L"pcg";
    }
};
//=============================================================================
} // namespace Nelson
//=============================================================================
