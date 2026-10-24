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
#include "Random64BitEngine.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// xoshiro256++ (Blackman and Vigna): xor-shift-rotate generator with a 256-bit
// state and a double addition output. The four state words are expanded from
// the seed with SplitMix64.
struct Xoshiro256ppEngine
{
    static constexpr size_t N = 4;
    //=============================================================================
    static inline uint64
    rotl(uint64 x, int k)
    {
        return (x << k) | (x >> (64 - k));
    }
    //=============================================================================
    static inline uint64
    next(uint64* s)
    {
        // work on locals: chained updates through memory are several times slower.
        uint64 s0 = s[0], s1 = s[1], s2 = s[2], s3 = s[3];
        const uint64 result = rotl(s0 + s3, 23) + s0;
        const uint64 t = s1 << 17;
        s2 ^= s0;
        s3 ^= s1;
        s1 ^= s2;
        s0 ^= s3;
        s2 ^= t;
        s[0] = s0;
        s[1] = s1;
        s[2] = s2;
        s[3] = rotl(s3, 45);
        return result;
    }
    //=============================================================================
    static void
    seed(uint64* s, uint64 seedValue)
    {
        uint64 x = seedValue;
        for (size_t k = 0; k < N; ++k) {
            s[k] = splitMix64Next(x);
        }
    }
    //=============================================================================
    // the all-zero state is a fixed point of the recurrence.
    static bool
    normalize(uint64* s)
    {
        return (s[0] | s[1] | s[2] | s[3]) != 0;
    }
    //=============================================================================
};
//=============================================================================
class RandomXoshiro256pp : public Random64BitEngine<Xoshiro256ppEngine>
{
public:
    std::wstring
    getGeneratorName() override
    {
        return L"xoshiro";
    }
};
//=============================================================================
} // namespace Nelson
//=============================================================================
