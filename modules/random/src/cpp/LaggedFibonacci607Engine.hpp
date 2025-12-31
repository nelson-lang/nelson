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
#include <array>
#include <cstdint>
#include <limits>
#include <random>
#include <istream>
#include <ostream>
//=============================================================================
// Minimal lagged Fibonacci 607,273 random engine (additive, mod 2^32)
class LaggedFibonacci607Engine
{
public:
    using result_type = uint32_t;
    static constexpr size_t k = 607;
    static constexpr size_t j = 273;
    static constexpr result_type default_seed = 5489u;
    //=============================================================================
    LaggedFibonacci607Engine(result_type s = default_seed) { seed(s); }
    //=============================================================================
    void
    seed(result_type s = default_seed)
    {
        // Use std::minstd_rand0 to fill the state
        std::minstd_rand0 seeder(s);
        for (size_t i = 0; i < k; ++i) {
            state_[i] = seeder();
        }
        idx_ = 0;
    }
    //=============================================================================
    result_type
    operator()()
    {
        // Additive lagged Fibonacci: x_n = x_{n-k} + x_{n-j} mod 2^32
        size_t i = idx_;
        size_t i_k = (i + k - k) % k; // always i
        size_t i_j = (i + k - j) % k;
        state_[i] = state_[i_k] + state_[i_j];
        result_type res = state_[i];
        idx_ = (idx_ + 1) % k;
        return res;
    }
    //=============================================================================
    static constexpr result_type
    min()
    {
        return std::numeric_limits<result_type>::min();
    }
    static constexpr result_type
    max()
    {
        return std::numeric_limits<result_type>::max();
    }
    //=============================================================================
    // Save/load state
    friend std::ostream&
    operator<<(std::ostream& os, const LaggedFibonacci607Engine& eng)
    {
        os << eng.idx_;
        for (size_t i = 0; i < k; ++i) {
            os << ' ' << eng.state_[i];
        }
        return os;
    }
    //=============================================================================
    friend std::istream&
    operator>>(std::istream& is, LaggedFibonacci607Engine& eng)
    {
        is >> eng.idx_;
        for (size_t i = 0; i < k; ++i) {
            is >> eng.state_[i];
        }
        return is;
    }
    //=============================================================================
private:
    std::array<result_type, k> state_ {};
    size_t idx_ = 0;
};
//=============================================================================
