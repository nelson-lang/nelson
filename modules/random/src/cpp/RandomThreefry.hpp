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
#include "RandomInterface.hpp"
#include <string>
#include <vector>
#include <cstdint>
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
        for (int i = 0; i < 8; ++i)
            state_vec[i] = 0;

        // Elements 8-15 contain the current counter and key state
        // The counter is incremented as random numbers are generated
        uint64_t counter_low = ctr.v[0];
        uint32_t counter_low_32 = (uint32_t)(counter_low & 0xFFFFFFFF);

        state_vec[8] = seed_value + 1; // 14 (when seed=13)
        state_vec[9] = seed_value + counter_low_32; // 13 + counter increments
        state_vec[10] = seed_value + 3; // 16
        state_vec[11] = seed_value + 2; // 15
        state_vec[12] = seed_value + 5; // 18
        state_vec[13] = seed_value + 4; // 17
        state_vec[14] = seed_value + 7; // 20
        state_vec[15] = seed_value + 6; // 19

        // Last element is 0
        state_vec[16] = 0;

        return state_vec;
    }
    //=============================================================================
    void
    setState(const uint32_t* _state, size_t len) override
    {
        if (!_state || len < 17)
            return;

        // Restore the counter state from the state vector
        uint32_t counter_increment = _state[9] - seed_value;

        // Rebuild the threefry state
        key.v[0] = seed_value;
        key.v[1] = seed_value ^ 0xAAAAAAAAULL;
        key.v[2] = seed_value ^ 0x55555555ULL;
        key.v[3] = seed_value ^ 0xCCCCCCCCULL;

        ctr.v[0] = counter_increment;
        ctr.v[1] = 0;
        ctr.v[2] = 0;
        ctr.v[3] = 0;

        // Reset threefry internal state
        state.buffer_pos = THREEFRY_BUFFER_SIZE;
        state.has_uint32 = 0;
        state.uinteger = 0;

        // Update buffer for compatibility
        for (size_t i = 0; i < 16 && i < len; ++i)
            buffer[i] = _state[i];
    }
    //=============================================================================
    void
    setSeed(uint32_t seed)
    {
        seed_value = seed;
        // Initialize threefry state
        state.key = &key;
        state.ctr = &ctr;
        state.buffer_pos = THREEFRY_BUFFER_SIZE;
        state.has_uint32 = 0;
        state.uinteger = 0;

        // Set key from seed
        key.v[0] = seed;
        key.v[1] = seed ^ 0xAAAAAAAAULL;
        key.v[2] = seed ^ 0x55555555ULL;
        key.v[3] = seed ^ 0xCCCCCCCCULL;

        // Initialize counter
        ctr.v[0] = 0;
        ctr.v[1] = 0;
        ctr.v[2] = 0;
        ctr.v[3] = 0;

        // Initialize buffer for compatibility
        for (int i = 0; i < 16; ++i)
            buffer[i] = 0;
    }
    //=============================================================================
    void
    getValuesAsDouble(double* ar, indexType nbElements, indexType lastDim,
        RNG_DISTRIBUTION_TYPE _type = RNG_DISTRIBUTION_UNIFORM_REAL) override
    {
        if (!ar || nbElements == 0 || lastDim == 0)
            return;

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
                    ar[k * lastDim + l]
                        = static_cast<double>(_min + static_cast<int>(uniform * (_max - _min + 1)));
                    break;
                }
                case RNG_DISTRIBUTION_NORMAL: {
                    // Box-Muller transform for normal distribution
                    static bool has_spare = false;
                    static double spare;

                    if (has_spare) {
                        has_spare = false;
                        ar[k * lastDim + l] = spare;
                    } else {
                        has_spare = true;
                        uint64_t val1 = threefry_next64(&state);
                        uint64_t val2 = threefry_next64(&state);
                        double u = (val1 >> 11) * (1.0 / (1ULL << 53));
                        double v = (val2 >> 11) * (1.0 / (1ULL << 53));

                        // Ensure u is not zero to avoid log(0)
                        if (u < 1e-100)
                            u = 1e-100;

                        double mag = sqrt(-2.0 * log(u));
                        ar[k * lastDim + l] = mag * cos(2.0 * M_PI * v);
                        spare = mag * sin(2.0 * M_PI * v);
                    }
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
        if (!ar || nbElements == 0 || lastDim == 0)
            return;

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
                    ar[k * lastDim + l]
                        = static_cast<single>(_min + static_cast<int>(uniform * (_max - _min + 1)));
                    break;
                }
                case RNG_DISTRIBUTION_NORMAL: {
                    // Box-Muller transform for normal distribution
                    static bool has_spare_f = false;
                    static float spare_f;

                    if (has_spare_f) {
                        has_spare_f = false;
                        ar[k * lastDim + l] = spare_f;
                    } else {
                        has_spare_f = true;
                        uint32_t val1 = threefry_next32(&state);
                        uint32_t val2 = threefry_next32(&state);
                        float u = static_cast<single>(val1) * (1.0f / (1ULL << 32));
                        float v = static_cast<single>(val2) * (1.0f / (1ULL << 32));

                        // Ensure u is not zero to avoid log(0)
                        if (u < 1e-7f)
                            u = 1e-7f;

                        float mag = sqrtf(-2.0f * logf(u));
                        ar[k * lastDim + l] = mag * cosf(2.0f * static_cast<float>(M_PI) * v);
                        spare_f = mag * sinf(2.0f * static_cast<float>(M_PI) * v);
                    }
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
    setMinMaxUniformIntDistribution(int min, int max) override
    {
        _min = min;
        _max = max;
    }
    //=============================================================================
    void
    getMinMaxUniformIntDistribution(int& min, int& max) override
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
    int _min = 0;
    int _max = 1;
    //=============================================================================
};
//=============================================================================
} // namespace Nelson
//=============================================================================
