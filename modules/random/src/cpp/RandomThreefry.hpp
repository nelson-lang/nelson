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
#include "RandomInterface.hpp"
#include <string>
#include <vector>
#include <cstdint>
#include <cmath>
#include "threefry.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class RandomThreefry : public RandomInterface
{
public:
    //=============================================================================
    RandomThreefry(uint32_t seed = 0)
    {
        setSeed(seed);
        setMinMaxUniformIntDistribution(0, 1);
    }
    //=============================================================================
    virtual ~RandomThreefry() { }
    //=============================================================================
    std::wstring
    getGeneratorName() override
    {
        return L"threefry";
    }
    //=============================================================================
    uint32_t
    getSeed() override
    {
        return seed_value;
    }
    //=============================================================================
    std::vector<uint32_t>
    getState()
    {
        std::vector<uint32_t> state_vec(17, 0);
        // First 8 elements are zeros
        for (int i = 0; i < 8; ++i) {
            state_vec[i] = 0;
        }

        // Elements 8-15 contain the current counter and key state
        // The counter is incremented as random numbers are generated
        uint64_t counter_low = ctr.v[0];
        uint32_t counter_low_32 = (uint32_t)(counter_low & 0xFFFFFFFF);

        state_vec[8] = seed_value + 1;
        state_vec[9] = seed_value + counter_low_32;
        state_vec[10] = seed_value + 3;
        state_vec[11] = seed_value + 2;
        state_vec[12] = seed_value + 5;
        state_vec[13] = seed_value + 4;
        state_vec[14] = seed_value + 7;
        state_vec[15] = seed_value + 6;

        state_vec[16] = 0;

        return state_vec;
    }
    //=============================================================================
    void
    setState(const uint32_t* _state, size_t len) override
    {
        if (!_state || len < 17) {
            return;
        }

        uint32_t counter_increment = _state[9] - seed_value;

        key.v[0] = seed_value;
        key.v[1] = seed_value ^ 0xAAAAAAAAULL;
        key.v[2] = seed_value ^ 0x55555555ULL;
        key.v[3] = seed_value ^ 0xCCCCCCCCULL;

        ctr.v[0] = counter_increment;
        ctr.v[1] = 0;
        ctr.v[2] = 0;
        ctr.v[3] = 0;

        state.buffer_pos = THREEFRY_BUFFER_SIZE;
        state.has_uint32 = 0;
        state.uinteger = 0;

        for (size_t i = 0; i < 16 && i < len; ++i) {
            buffer[i] = _state[i];
        }
        has_gauss = false;
    }
    //=============================================================================
    void
    setSeed(uint32_t seed) override
    {
        seed_value = seed;
        state.key = &key;
        state.ctr = &ctr;
        state.buffer_pos = THREEFRY_BUFFER_SIZE;
        state.has_uint32 = 0;
        state.uinteger = 0;

        key.v[0] = seed;
        key.v[1] = seed ^ 0xAAAAAAAAULL;
        key.v[2] = seed ^ 0x55555555ULL;
        key.v[3] = seed ^ 0xCCCCCCCCULL;

        ctr.v[0] = 0;
        ctr.v[1] = 0;
        ctr.v[2] = 0;
        ctr.v[3] = 0;

        for (int i = 0; i < 16; ++i) {
            buffer[i] = 0;
        }
        has_gauss = false;
        cached_gauss = 0.0;
    }
    //=============================================================================
    void
    getValuesAsDouble(double* ar, indexType nbElements, indexType lastDim,
        RNG_DISTRIBUTION_TYPE _type = RNG_DISTRIBUTION_UNIFORM_REAL) override
    {
        if (!ar || nbElements == 0 || lastDim == 0) {
            return;
        }

        size_t p = (nbElements / lastDim);
        for (size_t k = 0; k < p; k++) {
            for (indexType l = 0; l < lastDim; l++) {
                switch (_type) {
                case RNG_DISTRIBUTION_UNIFORM_REAL: {
                    uint64_t val = threefry_next64(&state);
                    ar[k * lastDim + l] = (val >> 11) * (1.0 / (1ULL << 53));
                    break;
                }
                case RNG_DISTRIBUTION_UNIFORM_INT: {
                    uint64_t val = threefry_next64(&state);
                    double uniform = (val >> 11) * (1.0 / (1ULL << 53));
                    ar[k * lastDim + l] = static_cast<double>(
                        _min + static_cast<int64>(uniform * static_cast<double>(_max - _min + 1)));
                    break;
                }
                case RNG_DISTRIBUTION_NORMAL: {
                    ar[k * lastDim + l] = nextNormal();
                    break;
                }
                default:
                    break;
                }
            }
        }
    }
    //=============================================================================
    void
    getValuesAsSingle(single* ar, indexType nbElements, indexType lastDim,
        RNG_DISTRIBUTION_TYPE _type = RNG_DISTRIBUTION_UNIFORM_REAL) override
    {
        if (!ar || nbElements == 0 || lastDim == 0) {
            return;
        }

        size_t p = (nbElements / lastDim);
        for (size_t k = 0; k < p; k++) {
            for (indexType l = 0; l < lastDim; l++) {
                switch (_type) {
                case RNG_DISTRIBUTION_UNIFORM_REAL: {
                    uint32_t val = threefry_next32(&state);
                    ar[k * lastDim + l] = static_cast<single>(val) * (1.0f / (1ULL << 32));
                    break;
                }
                case RNG_DISTRIBUTION_UNIFORM_INT: {
                    uint32_t val = threefry_next32(&state);
                    float uniform = static_cast<single>(val) * (1.0f / (1ULL << 32));
                    ar[k * lastDim + l] = static_cast<single>(
                        _min + static_cast<int64>(uniform * static_cast<double>(_max - _min + 1)));
                    break;
                }
                case RNG_DISTRIBUTION_NORMAL: {
                    ar[k * lastDim + l] = static_cast<single>(nextNormal());
                    break;
                }
                default:
                    break;
                }
            }
        }
    }
    //=============================================================================
    void
    setMinMaxUniformIntDistribution(int64 min, int64 max) override
    {
        _min = min;
        _max = max;
    }
    //=============================================================================
    void
    getMinMaxUniformIntDistribution(int64& min, int64& max) override
    {
        min = _min;
        max = _max;
    }
    //=============================================================================
    size_t
    getStateSize() override
    {
        return 17;
    }
    //=============================================================================
private:
    threefry_state state;
    threefry4x64_key_t key;
    threefry4x64_ctr_t ctr;
    uint32_t buffer[16] = { 0 };
    uint32_t seed_value = 0;
    int64 _min = 0;
    int64 _max = 1;
    bool has_gauss = false;
    double cached_gauss = 0.0;
    //=============================================================================
    double
    nextDouble53()
    {
        uint64_t val = threefry_next64(&state);
        return (val >> 11) * (1.0 / 9007199254740992.0);
    }
    //=============================================================================
    // Polar (Marsaglia) normal, consistent with the other engines. The spare
    // value is cached per instance (not in a shared static).
    double
    nextNormal()
    {
        if (has_gauss) {
            has_gauss = false;
            return cached_gauss;
        }
        double x1, x2, r2;
        do {
            x1 = 2.0 * nextDouble53() - 1.0;
            x2 = 2.0 * nextDouble53() - 1.0;
            r2 = x1 * x1 + x2 * x2;
        } while (r2 >= 1.0 || r2 == 0.0);
        double f = std::sqrt(-2.0 * std::log(r2) / r2);
        cached_gauss = f * x1;
        has_gauss = true;
        return f * x2;
    }
    //=============================================================================
};
//=============================================================================
} // namespace Nelson
//=============================================================================
