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
#include <cstddef>
#include <cstdint>
//=============================================================================
// MT19937-64 engine (same sequence as std::mt19937_64).
// The state is the ring of the 312 most recent words: its oldest-first order
// is the textual state of std::mt19937_64, so it is read and restored by a
// plain copy.
class MersenneTwister64Engine
{
public:
    using result_type = uint64_t;
    static constexpr size_t stateSize = 312;
    static constexpr result_type default_seed = 5489u;
    //=============================================================================
    explicit MersenneTwister64Engine(result_type s = default_seed) { seed(s); }
    //=============================================================================
    void
    seed(result_type s = default_seed)
    {
        words[0] = s;
        for (size_t k = 1; k < stateSize; ++k) {
            words[k] = 6364136223846793005ULL * (words[k - 1] ^ (words[k - 1] >> 62)) + k;
        }
        oldest = 0;
    }
    //=============================================================================
    result_type
    operator()()
    {
        const size_t next = (oldest + 1 == stateSize) ? 0 : oldest + 1;
        const size_t middle = (oldest + shiftSize < stateSize) ? oldest + shiftSize
                                                               : oldest + shiftSize - stateSize;
        const uint64_t x = (words[oldest] & upperMask) | (words[next] & lowerMask);
        uint64_t y = words[middle] ^ (x >> 1) ^ ((x & 1) ? xorMask : 0);
        words[oldest] = y;
        oldest = next;
        y ^= (y >> 29) & 0x5555555555555555ULL;
        y ^= (y << 17) & 0x71D67FFFEDA60000ULL;
        y ^= (y << 37) & 0xFFF7EEE000000000ULL;
        y ^= y >> 43;
        return y;
    }
    //=============================================================================
    static constexpr result_type
    min()
    {
        return 0;
    }
    //=============================================================================
    static constexpr result_type
    max()
    {
        return ~static_cast<result_type>(0);
    }
    //=============================================================================
    // out receives stateSize words, oldest first.
    void
    getState(uint64_t* out) const
    {
        size_t k = 0;
        for (size_t i = oldest; i < stateSize; ++i) {
            out[k++] = words[i];
        }
        for (size_t i = 0; i < oldest; ++i) {
            out[k++] = words[i];
        }
    }
    //=============================================================================
    // in holds stateSize words, oldest first.
    template <typename T>
    void
    setState(const T* in)
    {
        for (size_t k = 0; k < stateSize; ++k) {
            words[k] = static_cast<uint64_t>(in[k]);
        }
        oldest = 0;
    }
    //=============================================================================
private:
    static constexpr size_t shiftSize = 156;
    static constexpr uint64_t xorMask = 0xB5026F5AA96619E9ULL;
    static constexpr uint64_t upperMask = 0xFFFFFFFF80000000ULL;
    static constexpr uint64_t lowerMask = 0x7FFFFFFFULL;
    uint64_t words[stateSize];
    size_t oldest = 0;
};
//=============================================================================
