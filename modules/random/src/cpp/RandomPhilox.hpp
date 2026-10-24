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
#include <philox4x32_10.h>
//=============================================================================
namespace Nelson {
//=============================================================================
// Philox 4x32-10 counter-based generator (Random123). Uniform doubles use the
// full 53-bit mantissa (two 32-bit outputs); randn uses the polar (Marsaglia)
// method so that the normal distribution is consistent with the other engines.
class RandomPhilox : public RandomInterface
{
public:
    //=============================================================================
    RandomPhilox(uint32_t seed = 0)
    {
        setSeed(seed);
        setMinMaxUniformIntDistribution(0, 0xFFFFFFFF);
    }
    //=============================================================================
    virtual ~RandomPhilox() { }
    //=============================================================================
    std::wstring
    getGeneratorName() override
    {
        return L"philox";
    }
    //=============================================================================
    uint32_t
    getSeed() override
    {
        return seed_value;
    }
    //=============================================================================
    std::vector<uint32_t>
    getState() const
    {
        std::vector<uint32_t> state(7);
        state[0] = counter[0];
        state[1] = counter[1];
        state[2] = counter[2];
        state[3] = counter[3];
        state[4] = key[0];
        state[5] = key[1];
        state[6] = buf_pos;
        return state;
    }
    //=============================================================================
    void
    setState(const uint32_t* _state, size_t len) override
    {
        if (len >= 7) {
            counter[0] = _state[0];
            counter[1] = _state[1];
            counter[2] = _state[2];
            counter[3] = _state[3];
            key[0] = _state[4];
            key[1] = _state[5];
            buf_pos = (_state[6] <= 4) ? _state[6] : 4;
            buffer_valid = false;
            has_gauss = false;
        }
    }
    //=============================================================================
    void
    setSeed(uint32_t seed) override
    {
        seed_value = seed;
        counter[0] = 0;
        counter[1] = 0;
        counter[2] = seed;
        counter[3] = seed + 1;
        key[0] = seed + 2;
        key[1] = seed + 3;
        buf_pos = 4;
        buffer_valid = false;
        has_gauss = false;
        cached_gauss = 0.0;
    }
    //=============================================================================
    void
    getValuesAsDouble(double* ar, indexType nbElements, indexType lastDim,
        RNG_DISTRIBUTION_TYPE _type = RNG_DISTRIBUTION_UNIFORM_REAL) override
    {
        for (indexType i = 0; i < nbElements; ++i) {
            switch (_type) {
            case RNG_DISTRIBUTION_UNIFORM_REAL:
                ar[i] = nextDouble53();
                break;
            case RNG_DISTRIBUTION_NORMAL:
                ar[i] = nextNormal();
                break;
            case RNG_DISTRIBUTION_UNIFORM_INT:
                ar[i] = static_cast<double>(nextInt());
                break;
            default:
                ar[i] = std::nan("");
                break;
            }
        }
    }
    //=============================================================================
    void
    getValuesAsSingle(single* ar, indexType nbElements, indexType lastDim,
        RNG_DISTRIBUTION_TYPE _type = RNG_DISTRIBUTION_UNIFORM_REAL) override
    {
        for (indexType i = 0; i < nbElements; ++i) {
            switch (_type) {
            case RNG_DISTRIBUTION_UNIFORM_REAL:
                ar[i] = static_cast<single>(nextDouble53());
                break;
            case RNG_DISTRIBUTION_NORMAL:
                ar[i] = static_cast<single>(nextNormal());
                break;
            case RNG_DISTRIBUTION_UNIFORM_INT:
                ar[i] = static_cast<single>(nextInt());
                break;
            default:
                ar[i] = static_cast<single>(std::nan(""));
                break;
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
        return 7;
    }
    //=============================================================================
private:
    uint32_t counter[4] = { 0, 0, 0, 1 };
    uint32_t key[2] = { 2, 3 };
    uint32_t buffer[4] = { 0 };
    uint32_t buf_pos = 4;
    bool buffer_valid = false;
    uint32_t seed_value = 0;
    bool has_gauss = false;
    double cached_gauss = 0.0;
    int64 _min = 0;
    int64 _max = 0xFFFFFFFF;
    //=============================================================================
    void
    incrementCounter()
    {
        if (++counter[0] == 0) {
            if (++counter[1] == 0) {
                if (++counter[2] == 0) {
                    ++counter[3];
                }
            }
        }
    }
    //=============================================================================
    uint32_t
    nextUint32()
    {
        if (buf_pos >= 4) {
            if (buffer_valid) {
                incrementCounter();
            }
            buffer[0] = counter[0];
            buffer[1] = counter[1];
            buffer[2] = counter[2];
            buffer[3] = counter[3];
            uint32_t working_key[2] = { key[0], key[1] };
            philox4x32_10_alt(buffer, working_key);
            buffer_valid = true;
            buf_pos = 0;
        }
        return buffer[buf_pos++];
    }
    //=============================================================================
    double
    nextDouble53()
    {
        uint32_t a = nextUint32();
        uint32_t b = nextUint32();
        return ((a >> 5) * 67108864.0 + (b >> 6)) / 9007199254740992.0;
    }
    //=============================================================================
    int64
    nextInt()
    {
        return _min + static_cast<int64>(nextDouble53() * static_cast<double>(_max - _min + 1));
    }
    //=============================================================================
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
